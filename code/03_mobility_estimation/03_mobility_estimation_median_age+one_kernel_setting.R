library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")
load("data/gpr/interpol_grid_median.RData")

#### spatial origin ####

origin_grid <- mobest::search_spatial_origin(
  independent = list(
    age_median = mobest::create_spatpos(
      id = janno_final$Individual_ID,
      x = janno_final$x,
      y = janno_final$y,
      z = janno_final$Date_BC_AD_Median_Derived
    )
  ),
  dependent = mobest::create_obs(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  interpol_grid = interpol_grid,
  rearview_distance = 300
)

origin_grid <- origin_grid %>%
  dplyr::left_join(
    janno_final %>% dplyr::select(Individual_ID, region_id),
    by = c("search_id" = "Individual_ID")
  )

library(ggplot2)
origin_grid %>%
  ggplot(aes(x = search_z, y = spatial_distance)) +
  geom_point() +
  facet_wrap(~region_id) +
  geom_smooth()

#### mobility proxy ####
save(origin_grid, file = "data/mobility_estimation/origin_grid_median.RData")
