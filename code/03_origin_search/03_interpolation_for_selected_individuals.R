library(magrittr)

#### data ####

load("data/genotype_data/janno_final.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
load("data/origin_search/default_kernset.RData")

#### prepare inputs ####

janno_search <- janno_final %>%
  dplyr::mutate(
    search_z = Date_BC_AD_Median_Derived
  ) %>% dplyr::filter(
    Poseidon_ID %in% c(
      "Stuttgart_published.DG",
      "RISE434.SG",
      "3DT26.SG",
      "SI-40.SG"
    )
  ) %>%
  dplyr::mutate(
    Poseidon_ID = factor(Poseidon_ID, c(
      "Stuttgart_published.DG",
      "RISE434.SG",
      "3DT26.SG",
      "SI-40.SG"
    )),
    search_id = Poseidon_ID
  )

rearview_dists <- c(-500, -300, 0, 0)

spatial_pred_grid <- mobest::create_prediction_grid(
  extended_area,
  spatial_cell_size = 30000
)

#### run search ####

location_examples <- purrr::map2_dfr(
  janno_search %>% dplyr::group_split(Poseidon_ID), rearview_dists,
  function(janno_search, rearview) {
    mobest::locate(
      independent = mobest::create_spatpos(
        id = janno_final$Poseidon_ID,
        x = janno_final$x,
        y = janno_final$y,
        z = janno_final$Date_BC_AD_Median_Derived
      ),
      dependent = mobest::create_obs(
        C1_mds_u = janno_final$C1_mds_u,
        C2_mds_u = janno_final$C2_mds_u,
        C1_pca_proj_u = janno_final$C1_pca_proj_u,
        C2_pca_proj_u = janno_final$C2_pca_proj_u,
        C3_pca_proj_u = janno_final$C3_pca_proj_u,
        C4_pca_proj_u = janno_final$C4_pca_proj_u,
        C5_pca_proj_u = janno_final$C5_pca_proj_u
      ),
      kernel = default_kernset,
      search_independent = mobest::create_spatpos(
        id = janno_search$Poseidon_ID,
        x = janno_search$x,
        y = janno_search$y,
        z = janno_search$Date_BC_AD_Median_Derived
      ),
      search_dependent = mobest::create_obs(
        C1_mds_u = janno_search$C1_mds_u,
        C2_mds_u = janno_search$C2_mds_u,
        C1_pca_proj_u = janno_search$C1_pca_proj_u,
        C2_pca_proj_u = janno_search$C2_pca_proj_u,
        C3_pca_proj_u = janno_search$C3_pca_proj_u,
        C4_pca_proj_u = janno_search$C4_pca_proj_u,
        C5_pca_proj_u = janno_search$C5_pca_proj_u
      ),
      search_space_grid = spatial_pred_grid,
      search_time = rearview
    )
  }
)

#### prepare probability products

# mds
location_examples_C1toC2_mds_u <- location_examples %>%
  dplyr::filter(dependent_var_id %in% c(
    "C1_mds_u", "C2_mds_u"
  )) %>%
  mobest::multiply_dependent_probabilities()

# pca
location_examples_C1toC2_pca_proj_u <- location_examples %>%
  dplyr::filter(dependent_var_id %in% c(
    "C1_pca_proj_u", "C2_pca_proj_u"
  )) %>%
  mobest::multiply_dependent_probabilities()

location_examples_C1toC3_pca_proj_u <- location_examples %>%
  dplyr::filter(dependent_var_id %in% c(
    "C1_pca_proj_u", "C2_pca_proj_u", "C3_pca_proj_u"
  )) %>%
  mobest::multiply_dependent_probabilities()

location_examples_C1toC4_pca_proj_u <- location_examples %>%
  dplyr::filter(dependent_var_id %in% c(
    "C1_pca_proj_u", "C2_pca_proj_u", "C3_pca_proj_u", "C4_pca_proj_u"
  )) %>%
  mobest::multiply_dependent_probabilities()

location_examples_C1toC5_pca_proj_u <- location_examples %>%
  dplyr::filter(dependent_var_id %in% c(
    "C1_pca_proj_u", "C2_pca_proj_u", "C3_pca_proj_u", "C4_pca_proj_u", "C5_pca_proj_u"
  )) %>%
  mobest::multiply_dependent_probabilities()

#### output ####

save(janno_search, file = "data/origin_search/janno_search.RData")
save(
  location_examples,
  location_examples_C1toC2_mds_u,
  location_examples_C1toC2_pca_proj_u,
  location_examples_C1toC3_pca_proj_u,
  location_examples_C1toC4_pca_proj_u,
  location_examples_C1toC5_pca_proj_u,
  file = "data/origin_search/location_examples.RData"
)
