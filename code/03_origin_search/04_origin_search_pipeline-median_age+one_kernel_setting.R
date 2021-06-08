library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/search_area.RData")
load("data/origin_search/default_kernel.RData")

#### prepare model grid ####

model_grid <- mobest::create_model_grid(
  independent = mobest::create_spatpos_multi(
    id = janno_final$Individual_ID,
    x = list(janno_final$x),
    y = list(janno_final$y),
    z = list(janno_final$Date_BC_AD_Median_Derived),
    it = "age_median"
  ),
  dependent = mobest::create_obs(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  kernel = default_kernel,
  prediction_grid = list(
    scs100_tl50 = mobest::prediction_grid_for_spatiotemporal_area(
      search_area,
      spatial_cell_size = 100000,
      temporal_layers = seq(-7500, 1500, 50)
    )
  )
)

save(model_grid, file = "data/gpr/model_grid_median.RData")

#### run interpolation on model grid ####

interpol_grid <- mobest::run_model_grid(model_grid)

# library(ggplot2)
# interpol_grid %>%
#   dplyr::filter(
#     dependent_var_id == "C1",
#     z %in% seq(-7000, -1000, 500)
#   ) %>%
#   ggplot() +
#   geom_raster(aes(x, y, fill = mean, alpha = sd)) +
#   facet_wrap(~z) +
#   scale_fill_viridis_c() +
#   scale_alpha_continuous(range = c(1, 0), na.value = 0)

save(interpol_grid, file = "data/gpr/interpol_grid_median.RData")

#### calculate change ####
# interpol_grid_with_change <- interpol_grid %>%
#   dplyr::group_by(
#     kernel_setting_id, dependent_var_id, x, y
#   ) %>%
#   dplyr::arrange(z, .by_group = TRUE) %>%
#   dplyr::mutate(
#     change = mean - dplyr::lag(mean)
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(
#     dependent_var_id
#   ) %>%
#   dplyr::mutate(
#     sd_norm = ifelse(
#       dependent_var_id == "C1",
#       sd/diff(c(min(janno_final$C1), max(janno_final$C1))),
#       sd/diff(c(min(janno_final$C2), max(janno_final$C2)))
#     )
#   ) %>% 
#   dplyr::ungroup() %>%
#   tidyr::pivot_wider(
#     id_cols = c("kernel_setting_id", "region_id", "x", "y", "z"),
#     names_from = dependent_var_id,
#     values_from = c("change", "sd_norm")
#   ) %>% 
#   # dplyr::filter(
#   #   dplyr::across(tidyselect::starts_with("change_"), ~!is.na(.x))
#   # ) %>%
#   dplyr::mutate(
#     change_combined = sqrt(change_C1^2 + change_C2^2),
#     mean_sd_norm = (sd_norm_C1 + sd_norm_C2)/2
#   )
# 
# save(interpol_grid_with_change, file = "data/gpr/interpol_grid_median_with_change.RData")
# 
# temporal_change <- interpol_grid_with_change %>%
#   dplyr::group_by(
#     kernel_setting_id, region_id, z
#   ) %>%
#   dplyr::summarise(
#     mean_change_combined = median(change_combined),
#     mean_sd_norm = mean(mean_sd_norm)
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(
#     movavg = slider::slide_dbl(mean_change_combined, mean, .before = 4, .after = 4)
#   )
# 
# save(temporal_change, file = "data/gpr/temporal_change_median.RData")

#### spatial origin ####

janno_search <- janno_final %>%
  dplyr::filter(!is.na(region_id)) %>%
  dplyr::mutate(
    search_z = Date_BC_AD_Median_Derived
  ) %>% dplyr::filter(
    search_z >= -7500 &
      search_z <= 1500
  )

origin_grid_median <- mobest::search_spatial_origin(
  independent = mobest::create_spatpos_multi(
    id = janno_search$Individual_ID,
    x = list(janno_search$x),
    y = list(janno_search$y),
    z = list(janno_search$search_z),
    it = "age_median"
  ),
  dependent = mobest::create_obs(
    C1 = janno_search$C1,
    C2 = janno_search$C2
  ),
  interpol_grid = interpol_grid,
  rearview_distance = 0
)

#### add region information ####

origin_grid_median <- origin_grid_median %>% 
  dplyr::mutate(
    spatial_distance = spatial_distance/1000
  ) %>%
  dplyr::left_join(
    janno_final %>% dplyr::select(Individual_ID, region_id),
    by = c("search_id" = "Individual_ID")
  ) %>%
  dplyr::filter(!is.na(region_id))

# library(ggplot2)
# origin_grid %>%
#   dplyr::mutate(
#     spatial_distance = spatial_distance/1000
#   ) %>%
#   ggplot(aes(x = search_z, y = spatial_distance, color = angle_deg)) +
#   geom_point() +
#   facet_wrap(~region_id) +
#   scale_color_viridis_c()

load("data/spatial/epsg3035.RData")
load("data/spatial/mobility_regions.RData")

origin_region_ids <- origin_grid_median %>%
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
origin_grid_median$origin_region_id <- mobility_regions$region_id[origin_region_ids]

#### mobility proxy ####
save(origin_grid_median, file = "data/origin_search/origin_grid_median.RData")
