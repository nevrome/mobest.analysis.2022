library(magrittr)

load("data/genotype_data/janno_final.RData")
load("data/spatial/epsg3035.RData")
load("data/origin_search/default_kernset.RData")

#### define spatial groups ####

janno_sf <- sf::st_as_sf(janno_final, coords = c("x", "y"), crs = epsg3035)
circle_centers <- tibble::tribble(
  ~setup, ~group, ~location, ~x, ~y,
  "I",   "A", "Iberia",      3100000, 2000000,
  "I",   "B", "Baltics",     5400000, 3900000,
  "II",  "A", "Italy",       4500000, 2000000,
  "II",  "B", "Scandinavia", 4500000, 3800000,
  "III", "A", "Balkans",     5300000, 2500000,
  "III", "B", "Britain",     3400000, 3500000
)
circle_centers_sf <- circle_centers %>%
  sf::st_as_sf(coords = c("x", "y"), crs = epsg3035)
circles_sf <- circle_centers_sf %>%
  sf::st_buffer(dist = 500000)

#### find intersection of groups and janno ####

janno_intersected <- sf::st_intersection(
  janno_sf,
  circles_sf
) %>% sf::st_drop_geometry()

#### run interpolation for center points of group areas ####
times <- seq(-7500, 1500, 100)

interpolation_circle_centers <- mobest::create_model_grid(
  independent = mobest::create_spatpos_multi(
    age_median = mobest::create_spatpos(
      id = janno_final$Poseidon_ID,
      x = janno_final$x,
      y = janno_final$y,
      z = janno_final$Date_BC_AD_Median_Derived
    )
  ),
  dependent = mobest::create_obs_multi(
    MDS2 = mobest::create_obs(
      C1_mds_u = janno_final$C1_mds_u
    )
  ),
  kernel = mobest::create_kernset_multi(
    default_kernel = default_kernset
  ),
  prediction_grid = mobest::create_spatpos_multi(
    circles = mobest::create_geopos(
      id = circle_centers$location,
      x = circle_centers$x,
      y = circle_centers$y
    ) %>% mobest::geopos_to_spatpos(times)
  )
) %>%
  mobest::run_model_grid() %>%
  dplyr::left_join(
    circle_centers,
    by = c("geo_id" = "location", "x", "y")
  )

#### store results ####

save(
  circle_centers_sf,
  circles_sf,
  janno_intersected,
  interpolation_circle_centers,
  file = "data/simulation/real_world_group_development.RData"
)
