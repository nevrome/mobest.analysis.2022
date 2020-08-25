# sbatch code/mobility_estimation/slurm_05_mobility_estimation_pipeline_age_resampling.sh

library(magrittr)

#### read parameters ####

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
#age_resampling_run <- 4
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
    ds400_dt200_g001 = list(d = c(400000, 400000, 200), g = 0.01, on_residuals = T, auto = F),
    ds600_dt300_g001 = list(d = c(600000, 600000, 300), g = 0.01, on_residuals = T, auto = F),
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

#### remove points with too high SD ####

# interpol_grid <- interpol_grid %>% dplyr::filter(
#   ifelse(
#     dependent_var_id == "C1",
#     sd/diff(c(min(janno_final$C1), max(janno_final$C1))) < 0.5,
#     sd/diff(c(min(janno_final$C2), max(janno_final$C2))) < 0.5
#   )
# )

#### spatial origin ####

interpol_grid_origin <- mobest::search_spatial_origin(interpol_grid, steps = 4)



#### mobility proxy ####

mobility_proxy <- mobest::estimate_mobility(interpol_grid_origin, mobility_regions)

save(mobility_proxy, file = paste0("data/mobility_estimation/age_resampling/mobility_proxy_", age_resampling_run, ".RData"))

# scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/mobility_estimation/age_resampling/* .
