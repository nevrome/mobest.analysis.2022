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
    ds600_dt1200_g004 = list(d = c(600000, 600000, 1200), g = 0.04, on_residuals = T, auto = F),
    ds500_dt850_g004 = list(d = c(500000, 500000, 850), g = 0.04, on_residuals = T, auto = F),
    ds600_dt600_g004 = list(d = c(600000, 600000, 600), g = 0.04, on_residuals = T, auto = F),
    ds400_dt400_g004 = list(d = c(400000, 400000, 400), g = 0.04, on_residuals = T, auto = F),
    ds800_dt400_g004 = list(d = c(800000, 800000, 400), g = 0.04, on_residuals = T, auto = F)
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
