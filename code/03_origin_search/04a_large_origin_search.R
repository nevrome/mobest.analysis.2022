# qsub code/03_origin_search/05b_sge_origin_search.sh

library(magrittr)

#### read parameters ####

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
age_resampling_run <- 5
age_resampling_run <- as.numeric(args[1])

#### data ####

load("data/genotype_data/janno_final.RData")
load("data/spatial/extended_area.RData")
load("data/origin_search/default_kernset_mds2.RData")
load("data/origin_search/retrospection_distances.RData")

janno_final$z <- purrr::map_int(janno_final$Date_BC_AD_Sample, function(x) {x[age_resampling_run]})

janno_search <- janno_final %>%
  dplyr::filter(
    region_id != "Other region",
    Date_BC_AD_Median_Derived >= -7300 &
      Date_BC_AD_Median_Derived <= 1500
  )# %>%
  #dplyr::slice_head(n = 5)

spatial_pred_grid <- mobest::create_prediction_grid(
  extended_area,
  spatial_cell_size = 100000
)

#### calculate locate probability ####

origin_grid <- mobest::locate_multi(
  independent = mobest::create_spatpos_multi(
    mobest::create_spatpos(
      id = janno_final$Poseidon_ID,
      x = janno_final$x,
      y = janno_final$y,
      z = janno_final$z
    ),
    .names = paste0("age_resampling_run_", age_resampling_run)
  ),
  dependent = mobest::create_obs_multi(
    MDS2 = mobest::create_obs(
      C1_mds_u = janno_final$C1_mds_u,
      C2_mds_u = janno_final$C2_mds_u
    )
  ),
  kernel = mobest::create_kernset_multi(
    MDS2 = default_kernset_mds2
  ),
  search_independent = mobest::create_spatpos_multi(
    mobest::create_spatpos(
      id = janno_search$Poseidon_ID,
      x = janno_search$x,
      y = janno_search$y,
      z = janno_search$z
    ),
    .names = paste0("age_resampling_run_", age_resampling_run)
  ),
  search_dependent = mobest::create_obs_multi(
    MDS2 = mobest::create_obs(
      C1_mds_u = janno_search$C1_mds_u,
      C2_mds_u = janno_search$C2_mds_u
    )
  ),
  search_space_grid = spatial_pred_grid,
  search_time = -retrospection_distances["default"],
  search_time_mode = "relative"
)

#### save result ####

save(origin_grid, file = paste0(
  "data/origin_search/age_resampling+one_kernel_setting/run_", age_resampling_run, ".RData"
))