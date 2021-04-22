library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/origin_search/age_resampling+one_kernel_setting/origin_grid.RData")
load("data/origin_search/origin_grid_median.RData")
load("data/spatial/epsg3035.RData")
load("data/spatial/mobility_regions.RData")

#### prepare main table ####

# age median data
origin_grid_median_modified <- origin_grid_median %>% 
  dplyr::mutate(
    spatial_distance = spatial_distance/1000
  ) %>%
  dplyr::left_join(
    janno_final %>% dplyr::select(Individual_ID, region_id),
    by = c("search_id" = "Individual_ID")
  ) %>%
  dplyr::filter(!is.na(region_id))

origin_region_ids <- origin_grid_median_modified %>%
  sf::st_as_sf(
    coords = c("origin_x", "origin_y"),
    crs = epsg3035
  ) %>%
  sf::st_intersects(
    ., mobility_regions
  ) %>%
  purrr::map_int(
    function(x) {
      if (length(x) > 0) {
        x
      } else {
        NA
      }
    }
  )

origin_grid_median_modified$origin_region_id <- mobility_regions$region_id[origin_region_ids]

# age resampling data
origin_grid_modified <- origin_grid %>% 
  dplyr::mutate(
    spatial_distance = spatial_distance/1000
  ) %>%
  dplyr::left_join(
    janno_final %>% dplyr::select(Individual_ID, region_id),
    by = c("search_id" = "Individual_ID")
  ) %>%
  dplyr::filter(!is.na(region_id))

# age and direction groups
r <- range(origin_grid$search_z)
age_groups_limts <- seq(round(r[1], -3), round(r[2], -3), 200)

cut_angle <- function(x) {
  dplyr::case_when(
    x >= 0   & x <  45  ~ "N",
    x >= 45  & x <  135 ~ "E",
    x >= 135 & x <  225 ~ "S",
    x >= 225 & x <  315 ~ "W",
    x >= 315 & x <= 360 ~ "N",
    TRUE ~ NA_character_
  )
}

origin_grid_modified <- origin_grid_modified %>%
  dplyr::mutate(
    search_z_cut = age_groups_limts[cut(search_z, age_groups_limts, labels = F)] + 100,
    angle_deg_cut = cut_angle(angle_deg)
  )

# moving average
std <- function(x) sd(x)/sqrt(length(x))

moving_window_step_resolution <- 50
future::plan(future::multisession)
moving_origin_grid <- furrr::future_map_dfr(
  unique(origin_grid_modified$region_id),
  function(region) {
    origin_per_region <- origin_grid_modified %>%
      dplyr::filter(region_id == region)
    age_median_origin_per_region <- origin_grid_median_modified %>%
      dplyr::filter(region_id == region)
    purrr::map2_df(
      seq(-8000, 1500, moving_window_step_resolution),
      seq(-7500, 2000, moving_window_step_resolution),
      function(start, end) {
        io <- dplyr::filter(
          origin_per_region,
          search_z >= start,
          search_z < end
        )
        age_median_io <- dplyr::filter(
          age_median_origin_per_region,
          search_z >= start,
          search_z < end
        )
        if (nrow(io) > 0) {
          tibble::tibble(
            z = mean(c(start, end)),
            region_id = region,
            undirected_mean_spatial_distance = mean(io$spatial_distance),
            directed_mean_spatial_distance = sqrt(
              mean(io$search_x - io$origin_x)^2 +
                mean(io$search_y - io$origin_y)^2
            ) / 1000,
            mean_angle_deg = mobest::vec2deg(
              c(mean(io$origin_x - io$search_x), mean(io$origin_y - io$search_y))
            ),
            # version based on the age resampling runs
            # std_spatial_distance = std(io$spatial_distance)
            # this does not make much sense
            # therefore: version based on the number of individual observations
            # with median age
            std_spatial_distance = if (nrow(age_median_io) >= 3) {
              std(age_median_io$spatial_distance)
            } else {
              Inf
            },
            sd_spatial_distance = if (nrow(age_median_io) >= 3) {
              sd(age_median_io$spatial_distance)
            } else {
              Inf
            }
          )
        } else {
          tibble::tibble(
            z = mean(c(start, end)),
            region_id = region,
            undirected_mean_spatial_distance = NA,
            directed_mean_spatial_distance = NA,
            mean_angle_deg = NA,
            std_spatial_distance = Inf,
            sd_spatial_distance = Inf
          )
        }
      }
    )
  }
)

# no data windows
no_data_windows <- moving_origin_grid %>%
  dplyr::group_by(region_id) %>% 
  dplyr::mutate(
    usd = tidyr::replace_na(undirected_mean_spatial_distance, 0),
    cumsum_undir_dist = cumsum(usd)
  ) %>%
  dplyr::filter(
    is.na(undirected_mean_spatial_distance)
  ) %>%
  dplyr::group_by(region_id, cumsum_undir_dist) %>%
  dplyr::summarise(
    min_date_not_covered = min(z) - moving_window_step_resolution,
    max_date_not_covered = max(z) + moving_window_step_resolution,
    .groups = "drop"
  ) %>%
  dplyr::select(-cumsum_undir_dist)

#### save output ####

save(origin_grid_median_modified, file = "data/origin_search/origin_grid_median_modified.RData")
save(origin_grid_modified, file = "data/origin_search/origin_grid_modified.RData")
save(moving_origin_grid, file = "data/origin_search/moving_origin_grid.RData")
save(no_data_windows, file = "data/origin_search/no_data_windows.RData")
