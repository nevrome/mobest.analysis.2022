# qsub code/03_origin_search/04b_sge_large_origin_search.shq

library(magrittr)

#### read parameters ####

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
sample_run <- 5
sample_run <- as.numeric(args[1])

#### data ####

load("data/genotype_data/janno_final.RData")
load("data/spatial/extended_area.RData")
load("data/origin_search/default_kernset.RData")
load("data/origin_search/retrospection_distances.RData")

age_resampling_runs <- 30 # change to 100 for final run!

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

janno_search <- dplyr::filter(
    janno_final,
    #region_id %in% c("Britain and Ireland", "Central Europe", "Italy", "Southeastern Europe"),
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
  spatial_cell_size = 100000 # adjust for final run!
)

#### calculate locate probability ####

probability_grid <- mobest::locate_multi(
  independent = janno_final_spatpos_multi,
  dependent = mobest::create_obs_multi(
    mobi = mobest::create_obs(
      C1_mds_u = janno_final$C1_mds_u,
      C2_mds_u = janno_final$C2_mds_u,
      C1_pca_proj_u = janno_final$C1_pca_proj_u,
      C2_pca_proj_u = janno_final$C2_pca_proj_u,
      C3_pca_proj_u = janno_final$C3_pca_proj_u,
      C4_pca_proj_u = janno_final$C4_pca_proj_u,
      C5_pca_proj_u = janno_final$C5_pca_proj_u
    )
  ),
  kernel = mobest::create_kernset_multi(
    mobi = default_kernset
  ),
  search_independent = janno_search_spatpos_multi,
  search_dependent = mobest::create_obs_multi(
    mobi = mobest::create_obs(
      C1_mds_u = janno_search$C1_mds_u,
      C2_mds_u = janno_search$C2_mds_u,
      C1_pca_proj_u = janno_search$C1_pca_proj_u,
      C2_pca_proj_u = janno_search$C2_pca_proj_u,
      C3_pca_proj_u = janno_search$C3_pca_proj_u,
      C4_pca_proj_u = janno_search$C4_pca_proj_u,
      C5_pca_proj_u = janno_search$C5_pca_proj_u
    )
  ),
  search_space_grid = spatial_pred_grid,
  search_time = -retrospection_distances,#["default"],
  search_time_mode = "relative"
)

prob_product_grid_mds2 <- probability_grid %>%
  dplyr::filter(dependent_var_id %in% c("C1_mds_u", "C2_mds_u")) %>%
  mobest::multiply_dependent_probabilities()
prob_product_grid_pca5 <- probability_grid %>%
  dplyr::filter(dependent_var_id %in% c(
    "C1_pca_proj_u", "C2_pca_proj_u", "C3_pca_proj_u", "C4_pca_proj_u", "C5_pca_proj_u"
  )) %>%
  mobest::multiply_dependent_probabilities()

# library(ggplot2)
# p <- prob_product_grid_pca5 %>%
#   ggplot() +
#   geom_raster(aes(x = field_x, y = field_y, fill = probability)) +
#   facet_wrap(~independent_table_id) +
#   geom_point(
#     data = janno_search,
#     mapping = aes(x = x, y = y), colour = "red"
#   )

origin_vectors_mds2 <- prob_product_grid_mds2 %>%
  mobest::determine_origin_vectors(independent_table_id, search_time) %>%
  dplyr::mutate(multivar_method = "mds2")
origin_vectors_pca5 <- prob_product_grid_pca5 %>%
  mobest::determine_origin_vectors(independent_table_id, search_time) %>%
  dplyr::mutate(multivar_method = "pca5")

# p + geom_point(
#     data = origin_vectors,
#     mapping = aes(x = field_x, y = field_y), colour = "orange"
#   )

#### save result ####
origin_vectors <- rbind(origin_vectors_mds2, origin_vectors_pca5)

save(origin_vectors, file = paste0(
  "data/origin_search/large_origin_search/ovs_sample_", sample_run, ".RData"
))
