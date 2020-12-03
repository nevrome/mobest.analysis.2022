# sbatch code/mobility_estimation/slurm_05_mobility_estimation_pipeline_age_resampling.sh

#### read parameters ####

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
age_resampling_run <- 6
age_resampling_run <- as.numeric(args[1]) + 1

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")
load("data/spatial/mobility_regions.RData")

main_pred_grid <- mobest::create_prediction_grid(
  area, mobility_regions,
  spatial_cell_size = 100000,
  time_layers = seq(-7500, 1500, 50)
)

delta_x <- 10 * 10000
delta_y <- 10 * 10000
delta_z <- 10

#### prepare pca model grid ####
model_grid <- mobest::create_model_grid(
  independent = stats::setNames(list(
    tibble::tibble(
      x = janno_final$x, 
      y = janno_final$y, 
      z = sapply(janno_final$Date_BC_AD_Sample, function(x){ x[age_resampling_run] })
    )
  ), paste0("age_sample_", age_resampling_run)),
  dependent = list(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  kernel = list(
    ds450_dt800_g006 = list(d = c(450000, 450000, 800), g = 0.06, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    main = main_pred_grid,
    offset_x = dplyr::mutate(main_pred_grid, x = x + delta_x),
    offset_y = dplyr::mutate(main_pred_grid, y = y + delta_y),
    offset_z = dplyr::mutate(main_pred_grid, z = z + delta_z)
  )
)

#### run interpolation on model grid ####

interpol_grid <- mobest::run_model_grid(model_grid)

# interpol_grid %>%
#   dplyr::filter(
#     pred_grid_id == "main",
#     z %% 500 == 0
#   ) %>%
#   tidyr::pivot_wider(
#     id_cols = c("point_id", "x", "y", "z", "kernel_setting_id"),
#     names_from = "dependent_var_id",
#     values_from = "mean"
#   ) %>%
#   dplyr::mutate(
#     C1_C2 = C1-C2
#   ) %>%
#   ggplot() +
#   geom_raster(aes(x, y, fill = C2)) +
#   facet_wrap(~z) +
#   scale_fill_viridis_c()

#### mobility ####

mobility_proxy <- mobest::estimate_mobility(
  interpol_grid, delta_x, delta_y, delta_z
)

save(mobility_proxy, file = paste0(
  "data/mobility_estimation/age_resampling+derivative_based+directed_speed+one_kernel_setting/run_", age_resampling_run, ".RData"
))

# library(ggplot2)
# mob %>% dplyr::filter(
#   z %% 300 == 0
# ) %>%
#   ggplot() +
#   geom_raster(aes(x, y, fill = J_final_outlier_removed)) +
#   facet_wrap(~z) +
#   scale_fill_gradientn(colours = c("blue", "yellow", "red"))
# 
# mob %>% dplyr::filter(
#   z %% 500 == 0
# ) %>%
#   ggplot() +
#   geom_raster(aes(x, y, fill = angle, alpha = J_final_outlier_removed)) +
#   facet_wrap(~z) +
#   scale_fill_gradientn(colours = c("blue", "green", "green", "red", "red", "yellow", "yellow", "blue"))
# 
# mob %>%
#   dplyr::group_by(
#     region_id, z
#   ) %>%
#   dplyr::summarise(
#     median_J_final = median(J_final),
#     mean_angle = mobest::mean_deg(angle)
#   ) %>%
#   ggplot() +
#   geom_line(aes(z, median_J_final, color = mean_angle), size = 1) +
#   facet_wrap(~region_id) +
#   scale_color_gradientn(colours = c("blue", "green", "green", "red", "red", "yellow", "yellow", "blue")) +
#   coord_cartesian(ylim = c(0,10))

