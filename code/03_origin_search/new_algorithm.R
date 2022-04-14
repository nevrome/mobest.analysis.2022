library(magrittr)

#### data ####

load("data/genotype_data/janno_final.RData")
load("data/spatial/search_area.RData")
load("data/origin_search/default_kernel.RData")

#### prepare model grid ####

# janno_search <- janno_final %>%
#   dplyr::mutate(
#     search_z = sapply(janno_final$Date_BC_AD_Sample, function(x){ x[1] })
#   ) %>% dplyr::filter(
#     region_id != "Other region",
#     search_z >= -7300 &
#       search_z <= 1500
#   )

janno_search <- janno_final %>%
  dplyr::mutate(
    search_z = sapply(janno_final$Date_BC_AD_Sample, function(x){ x[1] })
  ) %>% dplyr::filter(
    Poseidon_ID %in% c(
      "Stuttgart_published.DG" , 
      "RISE434.SG", 
      "3DT26.SG",
      "SI-40.SG"
    )
  )

search <- mobest::search_origin(
  independent = mobest::create_spatpos_multi(
    age_median = mobest::create_spatpos(
      id = janno_final$Poseidon_ID,
      x = janno_final$x,
      y = janno_final$y,
      z = janno_final$Date_BC_AD_Median_Derived
    )
  ),
  dependent = mobest::create_obs(
    C1_mds_u = janno_final$C1_mds_u,
    C2_mds_u = janno_final$C2_mds_u
  ),
  kernel = mobest::create_kernset_multi(
    kernel_1 = mobest::create_kernset(
      C1_mds_u = mobest::create_kernel(900000, 900000, 800, 0.07),
      C2_mds_u = mobest::create_kernel(900000, 900000, 800, 0.07)
    )
  ),
  search_independent = mobest::create_spatpos_multi(
    search_selection = mobest::create_spatpos(
      id = janno_search$Poseidon_ID,
      x = janno_search$x,
      y = janno_search$y,
      z = janno_search$Date_BC_AD_Median_Derived
    )
  ),
  search_dependent = mobest::create_obs(
    C1_mds_u = janno_search$C1_mds_u,
    C2_mds_u = janno_search$C2_mds_u
  ),
  search_space_grid = mobest::create_prediction_grid(
    search_area,
    spatial_cell_size = 50000
  ),
  rearview_distance = 700
)

huhu <- search %>%
  dplyr::filter(
    #id == "Stuttgart_published.DG",
    dependent_var_id == "C1_mds_u",
    independent_table_id == "search_selection",
    field_independent_table_id == "age_median",
    field_kernel_setting_id == "kernel_1"
  )

library(ggplot2)
huhu %>%
  ggplot() +
  geom_raster(
    aes(x = field_x, y = field_y, fill = probability)
  ) +
  facet_wrap(~id)
