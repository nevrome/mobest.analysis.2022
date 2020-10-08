# sbatch code/mobility_estimation/slurm_05_mobility_estimation_pipeline_age_resampling.sh

library(magrittr)

#### read parameters ####

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
age_resampling_run <- 3
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
    ds550_dt1050_g006 = list(d = c(550000, 550000, 1050), g = 0.06, on_residuals = T, auto = F),
    ds550_dt550_g006 = list(d = c(550000, 550000, 550), g = 0.06, on_residuals = T, auto = F),
    ds1050_dt550_g006 = list(d = c(1050000, 1050000, 550), g = 0.06, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    scs100_tl100 = mobest::create_prediction_grid(
      area, 
      mobility_regions,
      spatial_cell_size = 100000, 
      time_layers = seq(-7600, 1500, 100)
    )
  )
)

#### run interpolation on model grid ####

interpol_grid <- mobest::run_model_grid(model_grid)

#### spatial origin ####

interpol_grid_origin <- mobest::search_spatial_origin(interpol_grid, steps = 1)

#### mobility proxy ####

mobility_proxy <- mobest::estimate_mobility(interpol_grid_origin)

save(mobility_proxy, file = paste0("data/mobility_estimation/age_resampling/mobility_proxy_", age_resampling_run, ".RData"))

# scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/mobility_estimation/age_resampling/* .
