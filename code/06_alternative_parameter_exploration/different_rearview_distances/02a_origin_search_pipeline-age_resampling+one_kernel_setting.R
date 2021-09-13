# qsub code/03_origin_search/05b_sge_origin_search.sh

library(magrittr)

#### read parameters ####

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
age_resampling_run <- 5
age_resampling_run <- as.numeric(args[1])

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/search_area.RData")
load("data/origin_search/default_kernel.RData")
load("data/origin_search/retrospection_distance_retrovar.RData")

#### prepare model grid ####

model_grid <- mobest::create_model_grid(
  independent = mobest::create_spatpos_multi(
    id = janno_final$Individual_ID,
    x = list(janno_final$x),
    y = list(janno_final$y),
    z = list(Map(function(x) {x[age_resampling_run]}, janno_final$Date_BC_AD_Sample) %>% unlist()),
    it = "age_sample"
  ),
  dependent = mobest::create_obs(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  kernel = default_kernel,
  prediction_grid = list(
    scs100_tl50 = mobest::prediction_grid_for_spatiotemporal_area(
      search_area,
      spatial_cell_size = 100000,
      temporal_layers = seq(-8000, 1000, 50)
    )
  )
)

#### run interpolation on model grid ####

interpol_grid <- mobest::run_model_grid(model_grid)

#### spatial origin ####

origin_grid_list <- Map(
  function(retro) {
    
    janno_search <- janno_final %>%
      dplyr::mutate(
        search_z = sapply(janno_final$Date_BC_AD_Sample, function(x){ x[age_resampling_run] })
      ) %>% dplyr::filter(
        region_id != "Other region",
        search_z >= (-8000 + retro) &
          search_z <= 1500
      )
    
    mobest::search_spatial_origin(
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
      rearview_distance = retro
    ) %>%
      dplyr::mutate(
        age_resampling_run = age_resampling_run,
        retrospection_distance = retro
      )
    
  }, retrospection_distances
)

#### mobility proxy ####

save(origin_grid_list, file = paste0(
  "data/origin_search/age_resampling+one_kernel_setting/retrovar_run_", age_resampling_run, ".RData"
))
