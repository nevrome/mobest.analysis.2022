library(magrittr)

#### data ####

load("data/genotype_data/janno_final.RData")
load("data/spatial/epsg3035.RData")
load("data/origin_search/default_kernset.RData")

# individual points

Rome <- sf::st_as_sf(
  tibble::tibble(lon = 12.50, lat = 41.90),
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
) %>% sf::st_transform(crs = epsg3035) %>% sf::st_coordinates()

# Budapest <- sf::st_as_sf(
#   tibble::tibble(lon = 19.05, lat = 47.50),
#   coords = c("lon", "lat"),
#   crs = 4326,
#   remove = FALSE
# ) %>% sf::st_transform(crs = epsg3035) %>% sf::st_coordinates()

# Barcelona <- sf::st_as_sf(
#   tibble::tibble(lon = 2.17, lat = 41.40),
#   coords = c("lon", "lat"),
#   crs = 4326,
#   remove = FALSE
# ) %>% sf::st_transform(crs = epsg3035) %>% sf::st_coordinates()

London <- sf::st_as_sf(
  tibble::tibble(lon = -0.11, lat = 51.50),
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
) %>% sf::st_transform(crs = epsg3035) %>% sf::st_coordinates()

Jerusalem <- sf::st_as_sf(
  tibble::tibble(lon = 35.22, lat = 31.77),
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
) %>% sf::st_transform(crs = epsg3035) %>% sf::st_coordinates()

Riga <- sf::st_as_sf(
  tibble::tibble(lon = 24.11, lat = 56.95),
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
) %>% sf::st_transform(crs = epsg3035) %>% sf::st_coordinates()

times <- seq(-7500, 1500, 1000)
n_times <- length(times) 

make_spatpos <- function(x) {
  mobest::create_spatpos(1:n_times, rep(x[1], n_times), rep(x[2], n_times), times)
}

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
    MDS2 = mobest::create_obs(
      C1_mds_u = janno_final$C1_mds_u,
      C2_mds_u = janno_final$C2_mds_u
    )
  ),
  kernel = mobest::create_kernset_multi(
    default_kernel = default_kernset
  ),
  prediction_grid = mobest::create_spatpos_multi(
    Rome      = make_spatpos(Rome),
    #Budapest  = make_spatpos(Budapest),
    #Barcelona = make_spatpos(Barcelona),
    London    = make_spatpos(London),
    Jerusalem = make_spatpos(Jerusalem),
    Riga      = make_spatpos(Riga)
  )
)

#### run interpolation on model grid ####

interpol_grid_examples <- mobest::run_model_grid(model_grid)

save(interpol_grid_examples, file = "data/origin_search/interpol_grid_specific_places.RData")
