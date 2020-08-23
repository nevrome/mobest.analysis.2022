library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")

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
    ds200_dt400_g001 = list(d = c(200000, 200000, 400), g = 0.01, on_residuals = T, auto = F),
    ds400_dt800_g001 = list(d = c(400000, 400000, 800), g = 0.01, on_residuals = T, auto = F),
    ds800_dt1600_g001 = list(d = c(800000, 800000, 1600), g = 0.01, on_residuals = T, auto = F),
    ds200_dt100_g001 = list(d = c(200000, 200000, 100), g = 0.01, on_residuals = T, auto = F),
    ds400_dt200_g001 = list(d = c(400000, 400000, 200), g = 0.01, on_residuals = T, auto = F),
    ds800_dt400_g001 = list(d = c(800000, 800000, 400), g = 0.01, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    scs100_tl100 = mobest::create_prediction_grid(
      area,
      spatial_cell_size = 100000,
      time_layers = seq(-7500, 1500, 50)
    )
  )
)

#### run interpolation on model grid ####

model_grid_result <- mobest::run_model_grid(model_grid)

#### unnest prediction to get a point-wise prediction table ####

interpol_grid <- mobest::unnest_model_grid(model_grid_result)

# library(ggplot2)
# interpol_grid %>%
#   dplyr::filter(
#     #kernel_setting_id == "ds400_dt700_g001",
#     dependent_var_id == "C1",
#     z %% 500 == 0
#   ) %>%
#   ggplot() +
#   geom_raster(aes(x, y, fill = mean)) +
#   facet_wrap(~z) +
#   scale_fill_viridis_c()
# 
# interpol_grid %>%
#   dplyr::filter(
#     #kernel_setting_id == "ds400_dt700_g001",
#     dependent_var_id == "C2",
#     z %% 200 == 0
#   ) %>%
#   ggplot() +
#   geom_raster(aes(x, y, fill = mean)) +
#   facet_wrap(~z) +
#   scale_fill_viridis_c()


save(interpol_grid, file = "data/gpr/interpol_grid_median.RData")
