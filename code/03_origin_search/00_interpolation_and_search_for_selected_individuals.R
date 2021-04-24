library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")
load("data/spatial/epsg3035.RData")

#### select individuals ####

ioi <- c(
  "I3025",
  "Stuttgart_published.DG" , 
  "RISE434.SG", 
  "3DT26.SG",
  "VK326.SG",
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
model_grid <- mobest::create_model_grid(
  independent = mobest::create_spatpos_multi(
    id = janno_final$Individual_ID,
    x = list(janno_final$x),
    y = list(janno_final$y),
    z = list(janno_final$Date_BC_AD_Median_Derived),
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
    it = "ds500_dt800_g006"
  ),
  prediction_grid = list(
    scs100_tlspecific = mobest::prediction_grid_for_spatiotemporal_area(
      area,
      spatial_cell_size = 100000,
      temporal_layers = janno_search$z
    )
  )
)

#### run interpolation on model grid ####

interpol_grid_specific <- mobest::run_model_grid(model_grid)

#### relatedness grid ####

grid_list <- interpol_grid_specific %>% 
  dplyr::select(dependent_var_id, x, y, z, mean, id) %>%
  tidyr::pivot_wider(
    id_cols = c("id", "x", "y", "z"), names_from = "dependent_var_id", values_from = "mean"
  ) %>%
  dplyr::left_join(
    janno_search %>% dplyr::select(Individual_ID, Date_BC_AD_Median_Derived), 
    by = c("z" = "Date_BC_AD_Median_Derived")
  ) %>%
  dplyr::group_split(z)

distance_grid_examples <- purrr::map(seq_len(nrow(janno_search)), function(i) {
  js <- janno_search[i,]
  sg <- grid_list[[i]]
  sg %>%
    dplyr::mutate(
      gen_dist = sqrt((C1 - js$C1)^2 + (C2 - js$C2)^2),
      min_gen_dist = gen_dist == min(gen_dist)
    )
}) %>% dplyr::bind_rows()

save(janno_search, file = "data/origin_search/janno_search.RData")
save(distance_grid_examples, file = "data/origin_search/distance_grid_examples.RData")
