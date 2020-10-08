library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")
load("data/spatial/mobility_regions.RData")
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

#### prepare pca model grid ####
model_grid <- mobest::create_model_grid(
  independent = c(
    list(age_center = tibble::tibble(
      x = janno_final$x, 
      y = janno_final$y, 
      z = janno_final$Date_BC_AD_Median_Derived
    ))
  ),
  dependent = list(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  kernel = list(
    ds800_dt1400_g001 = list(d = c(550000, 550000, 1050), g = 0.06, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    Budapest = tibble::tibble(
      x = Budapest[1], 
      y = Budapest[2], 
      z = seq(-7500, 1500, 1000), 
      point_id = 1:length(z)
    ),
    London = tibble::tibble(
      x = London[1], 
      y = London[2], 
      z = seq(-7500, 1500, 1000), 
      point_id = 1:length(z)
    ),
    Jerusalem = tibble::tibble(
      x = Jerusalem[1], 
      y = Jerusalem[2], 
      z = seq(-7500, 1500, 1000), 
      point_id = 1:length(z)
    ),
    Rome = tibble::tibble(
      x = Rome[1], 
      y = Rome[2], 
      z = seq(-7500, 1500, 1000), 
      point_id = 1:length(z)
    ) 
  )
)

#### run interpolation on model grid ####

interpol_grid_examples <- mobest::run_model_grid(model_grid)

save(interpol_grid_examples, file = "data/gpr/interpol_grid_examples.RData")
