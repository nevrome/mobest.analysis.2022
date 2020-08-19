library(magrittr)

#### read parameters ####

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
age_resampling_run <- 1
age_resampling_run <- as.numeric(args[1]) + 1

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
        z = sapply(janno_final$Date_BC_AD_Sample, function(x){ x[age_resampling_run] })
      )
    ) %>% stats::setNames(paste0("age_sample_", age_resampling_run)),
  dependent = list(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  kernel = list(
    ds600_dt2600_g001 = list(d = c(600000, 600000, 2600), g = 0.01, on_residuals = T, auto = F),
    ds800_dt1400_g001 = list(d = c(800000, 800000, 1400), g = 0.01, on_residuals = T, auto = F),
    ds1300_dt1000_g001 = list(d = c(1300000, 1300000, 1000), g = 0.01, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    scs100_tl100 = mobest::create_prediction_grid(
      area, 
      spatial_cell_size = 100000, 
      time_layers = seq(-7500, 1500, 100)
    )
  )
)

#### run interpolation on model grid ####

model_grid_result <- mobest::run_model_grid(model_grid)

#### unnest prediction to get a point-wise prediction table ####

interpol_grid <- mobest::unnest_model_grid(model_grid_result)

#save(interpol_grid, file = "data/gpr/interpol_grid.RData")
save(interpol_grid, file = paste0("data/gpr/interpol_grid_", age_resampling_run, ".RData"))

#### spatial origin ####

interpol_grid_origin <- mobest::search_spatial_origin(interpol_grid, steps = 4)

#### mobility proxy ####

mobility_proxy <- mobest::estimate_mobility(interpol_grid_origin, mobility_regions)

save(mobility_proxy, file = paste0("data/mobility_estimation/mobility_proxy_", age_resampling_run, ".RData"))

# scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/mobility_estimation_main_run/* .