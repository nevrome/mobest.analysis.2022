# qsub code/03_origin_search/04b_sge_large_origin_search.shq

library(magrittr)

#### read parameters ####

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
sample_run <- 5
sample_run <- as.numeric(args[1])

#### data ####

load("data/genotype_data/janno_final.RData")
load("data/spatial/extended_area.RData")
load("data/origin_search/default_kernset_mds2.RData")
load("data/origin_search/retrospection_distances.RData")

age_resampling_runs <- 100

janno_final_spatpos <- purrr::map(
  seq_len(age_resampling_runs), function(age_resampling_run) {
    mobest::create_spatpos(
      id = janno_final$Poseidon_ID,
      x = janno_final$x,
      y = janno_final$y,
      z = purrr::map_int(janno_final$Date_BC_AD_Sample, function(x) {x[age_resampling_run]})
    )
  }
)

janno_final_spatpos_multi <- do.call(
  mobest::create_spatpos_multi,
  c(janno_final_spatpos, list(.names = paste0("age_resampling_run_", seq_len(age_resampling_runs))))
)

janno_search <- janno_final %>%
  dplyr::filter(
    region_id != "Other region",
    Date_BC_AD_Median_Derived >= -7300 &
      Date_BC_AD_Median_Derived <= 1500
  ) %>% dplyr::slice(sample_run)

janno_search_spatpos <- purrr::map(
  seq_len(age_resampling_runs), function(age_resampling_run) {
    mobest::create_spatpos(
      id = janno_search$Poseidon_ID,
      x = janno_search$x,
      y = janno_search$y,
      z = purrr::map_int(janno_search$Date_BC_AD_Sample, function(x) {x[age_resampling_run]})
    )
  }
)

janno_search_spatpos_multi <- do.call(
  mobest::create_spatpos_multi,
  c(janno_search_spatpos, list(.names = paste0("age_resampling_run_", seq_len(age_resampling_runs))))
)

spatial_pred_grid <- mobest::create_prediction_grid(
  extended_area,
  spatial_cell_size = 100000
)

#### calculate locate probability ####

probability_grid <- mobest::locate_multi(
  independent = janno_final_spatpos_multi,
  dependent = mobest::create_obs_multi(
    MDS2 = mobest::create_obs(
      C1_mds_u = janno_final$C1_mds_u,
      C2_mds_u = janno_final$C2_mds_u
    )
  ),
  kernel = mobest::create_kernset_multi(
    MDS2 = default_kernset_mds2
  ),
  search_independent = janno_search_spatpos_multi,
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

prob_product_grid <- mobest::multiply_dependent_probabilities(probability_grid)

origin_vectors <- mobest::determine_origin_vectors(prob_product_grid, independent_table_id)

#### save result ####

save(origin_vectors, file = paste0(
  "data/origin_search/age_resampling+one_kernel_setting/sample_", sample_run, ".RData"
))
