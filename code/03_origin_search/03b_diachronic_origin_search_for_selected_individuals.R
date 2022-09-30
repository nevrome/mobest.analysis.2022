library(magrittr)

#### data ####

load("data/genotype_data/janno_final.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
load("data/origin_search/default_kernset.RData")

#### prepare inputs ####

diachronic_janno_search <- janno_final %>%
  dplyr::mutate(
    search_z = Date_BC_AD_Median_Derived
  ) %>% dplyr::filter(
    Poseidon_ID %in% c(
      "Stuttgart_published.DG"
    )
  ) %>%
  dplyr::mutate(
    Poseidon_ID = factor(Poseidon_ID, c(
      "Stuttgart_published.DG"
    )),
    search_id = Poseidon_ID
  )

rearview_dists <- c(seq(-7500, -5500, 200), -5250)

spatial_pred_grid <- mobest::create_prediction_grid(
  extended_area,
  spatial_cell_size = 30000
)

#### run search ####

diachronic_locations <- mobest::locate(
  independent = mobest::create_spatpos(
    id = janno_final$Poseidon_ID,
    x = janno_final$x,
    y = janno_final$y,
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = mobest::create_obs(
    C1_mds_u = janno_final$C1_mds_u,
    C2_mds_u = janno_final$C2_mds_u
  ),
  kernel = default_kernset,
  search_independent = mobest::create_spatpos(
    id = diachronic_janno_search$Poseidon_ID,
    x = diachronic_janno_search$x,
    y = diachronic_janno_search$y,
    z = diachronic_janno_search$Date_BC_AD_Median_Derived
  ),
  search_dependent = mobest::create_obs(
    C1_mds_u = diachronic_janno_search$C1_mds_u,
    C2_mds_u = diachronic_janno_search$C2_mds_u
  ),
  search_space_grid = spatial_pred_grid,
  search_time = rearview_dists,
  search_time_mode = "absolute"
)

#### prepare probability products

diachronic_location_examples <- mobest::multiply_dependent_probabilities(diachronic_locations)

#### output ####

save(janno_search, file = "data/origin_search/diachronic_janno_search_selected_individuals.RData")
save(
  diachronic_location_examples,
  file = "data/origin_search/diachronic_search_result_selected_individuals.RData"
)
