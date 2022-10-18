library(magrittr)

#### data ####

load("data/genotype_data/janno_final.RData")
load("data/spatial/extended_area.RData")
load("data/origin_search/default_kernset.RData")

#### prepare model grid ####

model_grid <- mobest::create_model_grid(
  independent = mobest::create_spatpos_multi(
    age_median = mobest::create_spatpos(
      id = janno_final$Poseidon_ID,
      x = janno_final$x,
      y = janno_final$y,
      z = janno_final$Date_BC_AD_Median_Derived
    )
  ),
  dependent = mobest::create_obs_multi(
    MDS2PCAPROJ5 = mobest::create_obs(
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
    default_kernel = default_kernset
  ),
  prediction_grid = mobest::create_spatpos_multi(
    time_sequence = mobest::create_prediction_grid(
      extended_area,
      spatial_cell_size = 50000
    ) %>% mobest::geopos_to_spatpos(seq(-7000, 1000, 2000))
  )
)

#### run interpolation on model grid ####

interpol_grid <- mobest::run_model_grid(model_grid)

save(interpol_grid, file = "data/origin_search/interpol_grid_selected_timeslices.RData")
