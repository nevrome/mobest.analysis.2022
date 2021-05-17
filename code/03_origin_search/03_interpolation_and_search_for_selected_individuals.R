library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")
load("data/spatial/epsg3035.RData")
load("data/origin_search/default_kernel.RData")

#### select individuals ####

ioi <- c(
  "Stuttgart_published.DG" , 
  "RISE434.SG", 
  "3DT26.SG",
  "SI-40.SG"
)

janno_search <- janno_final %>%
  dplyr::mutate(
    z = Date_BC_AD_Median_Derived
  ) %>%
  dplyr::filter(
    Individual_ID %in% ioi
  ) %>%
  dplyr::mutate(
    Individual_ID = factor(Individual_ID, levels = ioi)
  ) %>%
  dplyr::arrange(Individual_ID)

#### prepare model grid ####
nr_of_resampling_runs <- 10

model_grid <- mobest::create_model_grid(
  independent = mobest::create_spatpos_multi(
    id = janno_final$Individual_ID,
    x = Map(function(i){janno_final$x}, seq_len(nr_of_resampling_runs)),
    y = Map(function(i){janno_final$y}, seq_len(nr_of_resampling_runs)),
    z = Map(
      function(i){
        Map(function(x) { x[i]}, janno_final$Date_BC_AD_Sample) %>% unlist()
      }, 
      seq_len(nr_of_resampling_runs)
    ),
    it = paste0("age_sample_", seq_len(nr_of_resampling_runs))
  ),
  dependent = mobest::create_obs(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  kernel = default_kernel,
  prediction_grid = list(
    scs100_tlspecific = mobest::prediction_grid_for_spatiotemporal_area(
      area,
      spatial_cell_size = 50000,
      temporal_layers = janno_search$z
    )
  )
)

#### run interpolation on model grid ####

interpol_grid_specific <- mobest::run_model_grid(model_grid)

#### relatedness grid ####

grid_list <- interpol_grid_specific %>% 
  dplyr::select(independent_table_id, dependent_var_id, x, y, z, mean, id) %>%
  tidyr::pivot_wider(
    id_cols = c("id", "x", "y", "z", "independent_table_id"), names_from = "dependent_var_id", values_from = "mean"
  ) %>%
  dplyr::left_join(
    janno_search %>% dplyr::select(Individual_ID, Date_BC_AD_Median_Derived, C1, C2), 
    by = c("z" = "Date_BC_AD_Median_Derived"),
    suffix = c("_grid", "_search")
  )

distance_grid_multi_resampling <- grid_list %>% 
  dplyr::mutate(
    gen_dist = sqrt((C1_grid - C1_search)^2 + (C2_grid - C2_search)^2)
  ) %>%
  dplyr::group_by(independent_table_id, Individual_ID) %>%
  dplyr::mutate(
    min_gen_dist = gen_dist == min(gen_dist)
  ) %>%
  dplyr::ungroup()

closest_points_examples <- distance_grid_multi_resampling %>% dplyr::filter(min_gen_dist)

distance_grid_examples <- distance_grid_multi_resampling %>%
  dplyr::group_by(id, Individual_ID) %>%
  dplyr::transmute(x, y, z, gen_dist = mean(gen_dist)) %>%
  dplyr::ungroup()

save(janno_search, file = "data/origin_search/janno_search.RData")
save(closest_points_examples, file = "data/origin_search/closest_points_examples.RData")
save(distance_grid_examples, file = "data/origin_search/distance_grid_examples.RData")

