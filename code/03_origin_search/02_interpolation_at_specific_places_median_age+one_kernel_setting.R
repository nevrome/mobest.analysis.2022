library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")
load("data/spatial/epsg3035.RData")
load("data/origin_search/default_kernel.RData")

# individual point
Budapest <- sf::st_as_sf(
  tibble::tibble(lon = 19.05, lat = 47.50),
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
) %>% sf::st_transform(crs = epsg3035) %>% sf::st_coordinates()

Barcelona <- sf::st_as_sf(
  tibble::tibble(lon = 2.17, lat = 41.40),
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
) %>% sf::st_transform(crs = epsg3035) %>% sf::st_coordinates()

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

Dnipro <- sf::st_as_sf(
  tibble::tibble(lon = 35.05, lat = 48.46),
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
) %>% sf::st_transform(crs = epsg3035) %>% sf::st_coordinates()

time_points <- seq(-7500, 1500, 1000)
n_time_points <- length(time_points) 

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
  kernel = default_kernel,
  prediction_grid = mobest::create_spatpos_multi(
    x = list(
      rep(Barcelona[1], n_time_points),
      rep(London[1], n_time_points),
      rep(Jerusalem[1], n_time_points),
      rep(Dnipro[1], n_time_points)
    ), 
    y = list(
      rep(Barcelona[2], n_time_points),
      rep(London[2], n_time_points),
      rep(Jerusalem[2], n_time_points),
      rep(Dnipro[2], n_time_points)
    ),
    z = rep(list(time_points), 4),
    id = 1:n_time_points,
    it = c("Barcelona", "London", "Jerusalem", "Dnipro")
  )
)

#### run interpolation on model grid ####

interpol_grid_examples <- mobest::run_model_grid(model_grid)

save(interpol_grid_examples, file = "data/gpr/interpol_grid_examples.RData")
