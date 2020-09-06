library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")

main_pred_grid <- mobest::create_prediction_grid(
  area,
  spatial_cell_size = 100000,
  time_layers = seq(-7500, 1500, 50)
)

delta_x <- 10
delta_y <- 10
delta_z <- 10

#### prepare pca model grid ####
model_grid <- mobest::create_model_grid(
  independent = list(
    tibble::tibble(
      x = janno_final$x, 
      y = janno_final$y, 
      z = janno_final$Date_BC_AD_Median_Derived
    )
  ) %>% stats::setNames("age_median"),
  dependent = list(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  kernel = list(
    ds600_dt300_g001 = list(d = c(1000000, 1000000, 1000), g = 0.1, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    main = main_pred_grid,
    offset_x = main_pred_grid %>% dplyr::mutate(x = x + delta_x * 1000),
    offset_y = main_pred_grid %>% dplyr::mutate(y = y + delta_y * 1000),
    offset_z = main_pred_grid %>% dplyr::mutate(z = z + delta_z)
  )
)

#### run interpolation on model grid ####

model_grid_result <- mobest::run_model_grid(model_grid)

#### unnest prediction to get a point-wise prediction table ####

interpol_grid <- mobest::unnest_model_grid(model_grid_result)

#### mobility ####

mob <- interpol_grid %>% tidyr::pivot_wider(
  id_cols = c("pred_grid_id", "point_id", "x", "y", "z"),
  names_from = "dependent_var_id", values_from = c("mean", "sd")
) %>% tidyr::pivot_wider(
  id_cols = c("point_id"),
  names_from = c("pred_grid_id"), values_from = c("mean_C1", "mean_C2", "sd_C1", "sd_C2")
) %>%
  dplyr::left_join(
    main_pred_grid
  ) %>%
  dplyr::mutate(
    # delta
    delta_x = delta_x,
    delta_y = delta_y,
    delta_z = delta_z,
    # partial derivatives deriv_x_C*
    deriv_x_C1 = (mean_C1_offset_x - mean_C1_main) / delta_x,
    deriv_x_C2 = (mean_C2_offset_x - mean_C2_main) / delta_x,
    # partial derivatives deriv_y_C*
    deriv_y_C1 = (mean_C1_offset_y - mean_C1_main) / delta_y,
    deriv_y_C2 = (mean_C2_offset_y - mean_C2_main) / delta_y,
    # partial derivatives deriv_z_C*
    deriv_z_C1 = (mean_C1_offset_z - mean_C1_main) / delta_z,
    deriv_z_C2 = (mean_C2_offset_z - mean_C2_main) / delta_z,
    # two directional speeds for each spatial direction x and y
    J_x_C1 = -deriv_z_C1/deriv_x_C1,
    J_y_C1 = -deriv_z_C1/deriv_y_C1,
    J_x_C2 = -deriv_z_C2/deriv_x_C2,
    J_y_C2 = -deriv_z_C2/deriv_y_C2,
    # one combined speed for C1 and C2
    J_x = (J_x_C1 + J_x_C2)/2,
    J_y = (J_y_C1 + J_y_C2)/2,
    # final strength of the speed
    J_final = sqrt(J_x^2 + J_y^2)
  ) %>%
  dplyr::select(
    point_id, x, y, z, sd_C1_main, J_x, J_y, J_final
  ) %>%
  dplyr::mutate(
    angle = unlist(Map(function(x,y) {mobest::vec2deg(c(x,y))}, J_x, J_y)),
    J_final_outlier_removed = ifelse(
      J_final > quantile(J_final, probs = 0.90), quantile(J_final, probs = 0.90), J_final
    )
  )

library(ggplot2)
mob %>% dplyr::filter(
  z %% 500 == 0
) %>%
  ggplot() +
  geom_raster(aes(x, y, fill = J_final_outlier_removed)) +
  facet_wrap(~z) +
  scale_fill_viridis_c()

mob %>% dplyr::filter(
  z %% 500 == 0
) %>%
  ggplot() +
  geom_raster(aes(x, y, fill = angle, alpha = J_final_outlier_removed)) +
  facet_wrap(~z) +
  scale_fill_gradientn(colours = c("blue", "green", "green", "red", "red", "yellow", "yellow", "blue"))

load("data/spatial/epsg102013.RData")
load("data/spatial/mobility_regions.RData")

mob_with_regions <- mob %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = epsg102013) %>%
  sf::st_intersection(mobility_regions) %>%
  sf::st_drop_geometry()

mob_with_regions %>%
  dplyr::group_by(
    region_id, z
  ) %>%
  dplyr::summarise(
    median_J_final = median(J_final),
    mean_angle = mobest::mean_deg(angle)
  ) %>%
  ggplot() +
  geom_line(aes(z, median_J_final, color = mean_angle), size = 1) +
  facet_wrap(~region_id) +
  scale_color_gradientn(colours = c("blue", "green", "green", "red", "red", "yellow", "yellow", "blue")) +
  coord_cartesian(ylim = c(0,10))

