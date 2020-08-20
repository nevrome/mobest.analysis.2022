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
    #ds600_dt2600_g001 = list(d = c(600000, 600000, 2600), g = 0.01, on_residuals = T, auto = F),
    ds800_dt1400_g001 = list(d = c(800000, 800000, 1400), g = 0.01, on_residuals = T, auto = F)
    #ds1300_dt1000_g001 = list(d = c(1300000, 1300000, 1000), g = 0.01, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    # scs100_tl100 = mobest::create_prediction_grid(
    #   area, 
    #   spatial_cell_size = 100000, 
    #   time_layers = seq(-7500, 1500, 100)
    # ),
    scs200_tl200 = mobest::create_prediction_grid(
      area, 
      spatial_cell_size = 150000, 
      time_layers = seq(-7500, 1500, 281.25)
    )
  )
)

#### run interpolation on model grid ####

model_grid_result <- mobest::run_model_grid(model_grid)

#### unnest prediction to get a point-wise prediction table ####

interpol_grid <- mobest::unnest_model_grid(model_grid_result)

#save(interpol_grid, file = "data/gpr/interpol_grid.RData")
save(interpol_grid, file = "data/gpr/interpol_grid_scs200_tl200.RData")

#### spatial origin ####

# interpol_grid_origin <- mobest::search_spatial_origin(interpol_grid, steps = 1)

#### mobility proxy ####

# mobility_proxy <- mobest::estimate_mobility(interpol_grid_origin, mobility_regions)
# 
# save(mobility_proxy, file = paste0("data/mobility_estimation/mobility_proxy_", age_resampling_run, ".RData"))
