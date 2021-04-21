# qsub code/03_origin_search/sge_origin_search.sh

library(magrittr)

#### read parameters ####

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
age_resampling_run <- 5
age_resampling_run <- as.numeric(args[1])

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")

#### prepare model grid ####

model_grid <- mobest::create_model_grid(
  independent = mobest::create_spatpos_multi(
    id = janno_final$Individual_ID,
    x = list(janno_final$x),
    y = list(janno_final$y),
    z = list(sapply(janno_final$Date_BC_AD_Sample, function(x){ x[age_resampling_run] })),
    it = "age_median"
  ),
  dependent = mobest::create_obs(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  kernel = mobest::create_kernset_multi(
    d = list(c(500000, 500000, 800)), 
    g = 0.06, 
    on_residuals = T, 
    auto = F,
    it = "ds500_dt800_g008"
  ),
  prediction_grid = list(
    scs100_tl50 = mobest::prediction_grid_for_spatiotemporal_area(
      area,
      spatial_cell_size = 100000,
      temporal_layers = seq(-7500, 1500, 50)
    )
  )
)

#### run interpolation on model grid ####

interpol_grid <- mobest::run_model_grid(model_grid)

#### spatial origin ####

janno_search <- janno_final %>%
  dplyr::mutate(
    search_z = sapply(janno_final$Date_BC_AD_Sample, function(x){ x[age_resampling_run] })
  ) %>% dplyr::filter(
    search_z >= -7500 &
      search_z <= 1500
    )

origin_grid <- mobest::search_spatial_origin(
  independent = mobest::create_spatpos_multi(
    id = janno_search$Individual_ID,
    x = list(janno_search$x),
    y = list(janno_search$y),
    z = list(janno_search$search_z),
    it = "age_sample"
  ),
  dependent = mobest::create_obs(
    C1 = janno_search$C1,
    C2 = janno_search$C2
  ),
  interpol_grid = interpol_grid,
  rearview_distance = 0
)

origin_grid$age_resampling_run <- age_resampling_run

#### mobility proxy ####

save(origin_grid, file = paste0(
  "data/origin_search/age_resampling+one_kernel_setting/run_", age_resampling_run, ".RData"
))
