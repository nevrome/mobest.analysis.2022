library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")

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
  kernel = mobest::create_kernset_multi(
    d = list(c(500000, 500000, 800)), 
    g = 0.08, 
    on_residuals = T, 
    auto = F,
    it = "ds500_dt800_g008"
  ),
  prediction_grid = list(
    scs100_tl50 = mobest::prediction_grid_for_spatiotemporal_area(
      area,
      spatial_cell_size = 100000,
      temporal_layers = seq(-8000, 1500, 50)
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

janno_post_7500 <- janno_final %>% dplyr::filter(
  Date_BC_AD_Median_Derived >= -7500
)

origin_grid_median <- mobest::search_spatial_origin(
  independent = mobest::create_spatpos_multi(
    id = janno_post_7500$Individual_ID,
    x = list(janno_post_7500$x),
    y = list(janno_post_7500$y),
    z = list(janno_post_7500$Date_BC_AD_Median_Derived),
    it = "age_median"
  ),
  dependent = mobest::create_obs(
    C1 = janno_post_7500$C1,
    C2 = janno_post_7500$C2
  ),
  interpol_grid = interpol_grid,
  rearview_distance = 300
)

# origin_grid_median <- origin_grid_median %>%
#   dplyr::left_join(
#     janno_final %>% dplyr::select(Individual_ID, region_id),
#     by = c("search_id" = "Individual_ID")
#   )

# library(ggplot2)
# origin_grid %>%
#   dplyr::mutate(
#     spatial_distance = spatial_distance/1000
#   ) %>%
#   ggplot(aes(x = search_z, y = spatial_distance, color = angle_deg)) +
#   geom_point() +
#   facet_wrap(~region_id) +
#   scale_color_viridis_c()

#### mobility proxy ####
save(origin_grid_median, file = "data/origin_search/origin_grid_median.RData")
