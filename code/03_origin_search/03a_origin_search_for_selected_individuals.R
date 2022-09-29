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
      #"I4874", # Iron gates HG
      
      "Stuttgart_published.DG",
      "RISE434.SG",
      "3DT26.SG",
      "SI-40.SG",
      
      # "MX265",
      # "I0099_published",
      # "RISE276.SG"
      
      # "FN2.SG", # Roman soldier North of the Alps with relations to Spain
      # "NO3423.SG",
      # "STR360c.SG",
      # "Alh10.SG"
      
      "I8341",
      "I8215"
    )
  ) %>%
  dplyr::mutate(
    Poseidon_ID = factor(Poseidon_ID, c(
      #"I4874",
      
      "Stuttgart_published.DG",
      "RISE434.SG",
      "3DT26.SG",
      "SI-40.SG",
      
      # "MX265",
      # "I0099_published",
      # "RISE276.SG"
      
      # "FN2.SG",
      # "NO3423.SG",
      # "STR360c.SG",
      # "Alh10.SG"
      
      "I8341",
      "I8215"
    )),
    search_id = Poseidon_ID
  )

rearview_dists <- c(-1500, -300, 0, 0, -100, -100)

spatial_pred_grid <- mobest::create_prediction_grid(
  extended_area,
  spatial_cell_size = 30000
)

#### run search ####

location_examples <- purrr::map2_dfr(
  janno_search %>% dplyr::group_split(Poseidon_ID), rearview_dists,
  function(janno_search, rearview) {
    message("Working on sample: ", janno_search$Poseidon_ID %>% unique)
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
        C5_pca_proj_u = janno_final$C5_pca_proj_u,
        C6_pca_proj_u = janno_final$C6_pca_proj_u,
        C7_pca_proj_u = janno_final$C7_pca_proj_u,
        C8_pca_proj_u = janno_final$C8_pca_proj_u,
        C9_pca_proj_u = janno_final$C9_pca_proj_u,
        C10_pca_proj_u = janno_final$C10_pca_proj_u
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
        C5_pca_proj_u = janno_search$C5_pca_proj_u,
        C6_pca_proj_u = janno_search$C6_pca_proj_u,
        C7_pca_proj_u = janno_search$C7_pca_proj_u,
        C8_pca_proj_u = janno_search$C8_pca_proj_u,
        C9_pca_proj_u = janno_search$C9_pca_proj_u,
        C10_pca_proj_u = janno_search$C10_pca_proj_u
      ),
      search_space_grid = spatial_pred_grid,
      search_time = rearview
    )
  }
)

#### prepare probability products

multiply_dims <- function(x) {
  location_examples %>%
    dplyr::filter(dependent_var_id %in% x) %>%
    mobest::multiply_dependent_probabilities()
}

# mds
location_examples_C1toC2_mds_u <- multiply_dims(c("C1_mds_u", "C2_mds_u"))

# pca
pca_vars <- c(
  "C1_pca_proj_u", "C2_pca_proj_u", "C3_pca_proj_u", "C4_pca_proj_u", "C5_pca_proj_u",
  "C6_pca_proj_u", "C7_pca_proj_u", "C8_pca_proj_u", "C9_pca_proj_u", "C10_pca_proj_u"
)

location_examples_C1toC2_pca_proj_u <- multiply_dims(pca_vars[1:2])
location_examples_C1toC3_pca_proj_u <- multiply_dims(pca_vars[1:3])
location_examples_C1toC4_pca_proj_u <- multiply_dims(pca_vars[1:4])
location_examples_C1toC5_pca_proj_u <- multiply_dims(pca_vars[1:5])
location_examples_C1toC6_pca_proj_u <- multiply_dims(pca_vars[1:6])
location_examples_C1toC7_pca_proj_u <- multiply_dims(pca_vars[1:7])
location_examples_C1toC8_pca_proj_u <- multiply_dims(pca_vars[1:8])
location_examples_C1toC9_pca_proj_u <- multiply_dims(pca_vars[1:9])
location_examples_C1toC10_pca_proj_u <- multiply_dims(pca_vars[1:10])

#### output ####

save(janno_search, file = "data/origin_search/janno_search_selected_individuals.RData")
save(
  location_examples,
  location_examples_C1toC2_mds_u,
  location_examples_C1toC2_pca_proj_u,
  location_examples_C1toC3_pca_proj_u,
  location_examples_C1toC4_pca_proj_u,
  location_examples_C1toC5_pca_proj_u,
  location_examples_C1toC6_pca_proj_u,
  location_examples_C1toC7_pca_proj_u,
  location_examples_C1toC8_pca_proj_u,
  location_examples_C1toC9_pca_proj_u,
  location_examples_C1toC10_pca_proj_u,
  file = "data/origin_search/search_result_selected_individuals.RData"
)
