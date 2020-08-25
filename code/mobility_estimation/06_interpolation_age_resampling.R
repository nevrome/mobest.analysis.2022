# sbatch code/mobility_estimation/slurm_06_interpolation_age_resampling.sh

library(magrittr)

#### read parameters ####

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
#age_resampling_run <- 4
age_resampling_run <- as.numeric(args[1]) + 1

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")

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
    ds600_dt300_g001 = list(d = c(600000, 600000, 300), g = 0.01, on_residuals = T, auto = F)
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

#### store result ####

save(interpol_grid, file = paste0("data/gpr/age_resampling/interpol_grid_", age_resampling_run, ".RData"))

# scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/gpr/age_resampling/* .
