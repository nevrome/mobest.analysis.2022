library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/origin_search/age_resampling+one_kernel_setting/origin_grid.RData")
load("data/origin_search/origin_grid_median.RData")
load("data/spatial/epsg3035.RData")
load("data/spatial/mobility_regions.RData")

#### prepare output data ####

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
age_groups_limits <- seq(-7250, 1750, 500)

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
    search_z_cut = age_groups_limits[cut(search_z, age_groups_limits, labels = F)] + 250,
  )

# means for search points across age resampling
origin_grid_mean <- origin_grid_modified %>%
  dplyr::group_by(
    search_id
  ) %>%
  dplyr::summarise(
    mean_search_z = mean(search_z),
    region_id = dplyr::first(region_id),
    undirected_mean_spatial_distance = mean(spatial_distance),
    undirected_2std_spatial_distance = sd(spatial_distance),
    directed_mean_spatial_distance = sqrt(
      mean(search_x - origin_x)^2 +
        mean(search_y - origin_y)^2
    ) / 1000,
    mean_angle_deg = mobest::vec2deg(
      c(mean(origin_x - search_x), mean(origin_y - search_y))
    ),
    mean_angle_deg_cut = cut_angle(mean_angle_deg),
    .groups = "drop"
  ) %>% dplyr::arrange(
    undirected_mean_spatial_distance
  )

# moving window
se <- function(x) sd(x)/sqrt(length(x))

moving_window_window_width <- 400
moving_window_step_resolution <- 50

future::plan(future::multisession)
moving_origin_grid <- furrr::future_map_dfr(
  # loop through all regions
  unique(origin_grid_modified$region_id),
  function(region) {
    origin_per_region <- origin_grid_modified %>%
      dplyr::filter(region_id == region)
    purrr::map2_df(
      # define moving windows and loop through them
      seq(-8000, 2000 - moving_window_window_width, moving_window_step_resolution),
      seq(-8000 + moving_window_window_width, 2000, moving_window_step_resolution),
      function(start, end) {
        # prepare window data subsets
        io <- dplyr::filter(
          origin_per_region,
          search_z >= start,
          search_z < end
        )
        io_upper_quartile <- dplyr::filter(
          io,
          spatial_distance >= quantile(spatial_distance, probs = 0.75)
        )
        io_run_grouped <- io %>% 
          dplyr::group_by(search_id) %>%
          dplyr::summarise(
            mean_spatial_distance = mean(spatial_distance),
            groups = "drop"
          )
        if (nrow(io) > 0) {
          tibble::tibble(
            z = mean(c(start, end)),
            region_id = region,
            undirected_mean_spatial_distance = 
              mean(io$spatial_distance),
            undirected_mean_spatial_distance_upper_quartile = 
              mean(io_upper_quartile$spatial_distance),
            directed_mean_spatial_distance = sqrt(
              mean(io$search_x - io$origin_x)^2 +
                mean(io$search_y - io$origin_y)^2
            ) / 1000,
            directed_mean_spatial_distance_upper_quartile = sqrt(
              mean(io_upper_quartile$search_x - io_upper_quartile$origin_x)^2 +
                mean(io_upper_quartile$search_y - io_upper_quartile$origin_y)^2
            ) / 1000,
            se_spatial_distance = if (nrow(io_run_grouped) >= 3) {
              se(io_run_grouped$mean_spatial_distance)
            } else {
              Inf
            },
            sd_spatial_distance = if (nrow(io_run_grouped) >= 3) {
              sd(io_run_grouped$mean_spatial_distance)
            } else {
              Inf
            },
            fraction_smaller_500 = sum(io$spatial_distance < 500) / nrow(io),
            fraction_bigger_500 = sum(io$spatial_distance >= 500 & io$spatial_distance < 1000) / nrow(io),
            fraction_bigger_1000 = sum(io$spatial_distance >= 1000 & io$spatial_distance < 2000) / nrow(io),
            fraction_bigger_2000 = sum(io$spatial_distance >= 2000) / nrow(io)
          )
        } else {
          tibble::tibble(
            z = mean(c(start, end)),
            region_id = region,
            undirected_mean_spatial_distance = NA,
            directed_mean_spatial_distance = NA,
            mean_angle_deg = NA,
            se_spatial_distance = Inf,
            sd_spatial_distance = Inf,
            fraction_smaller_500 = NA,
            fraction_bigger_500 = NA,
            fraction_bigger_1000 = NA,
            fraction_bigger_2000 = NA
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

save(origin_grid_modified, file = "data/origin_search/origin_grid_modified.RData")
save(origin_grid_mean, file = "data/origin_search/origin_grid_mean.RData")
save(moving_origin_grid, file = "data/origin_search/moving_origin_grid.RData")
save(no_data_windows, file = "data/origin_search/no_data_windows.RData")
