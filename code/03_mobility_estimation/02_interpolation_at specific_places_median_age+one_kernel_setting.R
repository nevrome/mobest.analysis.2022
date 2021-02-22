library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")
load("data/spatial/epsg102013.RData")

# individual point
Budapest <- sf::st_as_sf(
  tibble::tibble(lon = 19.05, lat = 47.50),
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
) %>% sf::st_transform(crs = epsg102013) %>% sf::st_coordinates()

London <- sf::st_as_sf(
  tibble::tibble(lon = -0.11, lat = 51.50),
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
) %>% sf::st_transform(crs = epsg102013) %>% sf::st_coordinates()

Jerusalem <- sf::st_as_sf(
  tibble::tibble(lon = 35.22, lat = 31.77),
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
) %>% sf::st_transform(crs = epsg102013) %>% sf::st_coordinates()

Rome <- sf::st_as_sf(
  tibble::tibble(lon = 12.50, lat = 41.90),
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
) %>% sf::st_transform(crs = epsg102013) %>% sf::st_coordinates()

time_points <- seq(-7500, 1500, 1000)
n_time_points <- length(time_points_to_measure) 

#### prepare pca model grid ####
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
    g = 0.08, 
    on_residuals = T, 
    auto = F,
    it = "ds500_dt800_g008"
  ),
  prediction_grid = mobest::create_spatpos_multi(
    x = list(
      rep(Budapest[1], n_time_points),
      rep(London[1], n_time_points),
      rep(Jerusalem[1], n_time_points),
      rep(Rome[1], n_time_points)
    ), 
    y = list(
      rep(Budapest[2], n_time_points),
      rep(London[2], n_time_points),
      rep(Jerusalem[2], n_time_points),
      rep(Rome[2], n_time_points)
    ),
    z = rep(list(time_points_to_measure), 4),
    id = 1:n_time_points,
    it = c("Budapest", "London", "Jerusalem", "Rome")
  )
)

#### run interpolation on model grid ####

interpol_grid_examples <- mobest::run_model_grid(model_grid)

save(interpol_grid_examples, file = "data/gpr/interpol_grid_examples.RData")
