library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")
load("data/spatial/mobility_regions.RData")

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
    ds550_dt1050_g006 = list(d = c(550000, 550000, 1050), g = 0.06, on_residuals = T, auto = F),
    ds550_dt550_g006 = list(d = c(550000, 550000, 550), g = 0.06, on_residuals = T, auto = F),
    ds1050_dt550_g006 = list(d = c(1050000, 1050000, 550), g = 0.06, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    scs100_tl100 = mobest::create_prediction_grid(
      area,
      mobility_regions,
      spatial_cell_size = 100000,
      time_layers = seq(-7500, 1500, 50)
    )
  )
)

#### run interpolation on model grid ####

interpol_grid <- mobest::run_model_grid(model_grid)

save(interpol_grid, file = "data/gpr/interpol_grid_median.RData")
