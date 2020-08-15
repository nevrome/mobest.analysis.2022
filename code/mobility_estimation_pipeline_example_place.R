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
    #ds200_dt200_g001 = list(d = c(200000, 200000, 200), g = 0.01, on_residuals = T, auto = F),
    ds500_dt500_g001 = list(d = c(500000, 500000, 500), g = 0.01, on_residuals = T, auto = F)
    #ds1000_dt1000_g001 = list(d = c(1000000, 1000000, 1000), g = 0.01, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    Budapest = tibble::tibble(
      x = Budapest[1], 
      y = Budapest[2], 
      z = seq(-8000, 1000, 1000), 
      point_id = 1:length(z)
    ),
    London = tibble::tibble(
      x = London[1], 
      y = London[2], 
      z = seq(-8000, 1000, 1000), 
      point_id = 1:length(z)
    ),
    Jerusalem = tibble::tibble(
      x = Jerusalem[1], 
      y = Jerusalem[2], 
      z = seq(-8000, 1000, 1000), 
      point_id = 1:length(z)
    ),
    Rome = tibble::tibble(
      x = Rome[1], 
      y = Rome[2], 
      z = seq(-8000, 1000, 1000), 
      point_id = 1:length(z)
    ) 
  )
)

#### run interpolation on model grid ####

model_grid_result <- mobest::run_model_grid(model_grid)

#### unnest prediction to get a point-wise prediction table ####

interpol_grid_examples <- mobest::unnest_model_grid(model_grid_result)

save(interpol_grid_examples, file = "data/gpr/interpol_grid_examples.RData")

# interpol_grid_spatial <- sf::st_as_sf(
#   interpol_grid,
#   coords = c("x", "y"),
#   crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
#   remove = FALSE
# )
# save(interpol_grid_spatial, file = "data/gpr/interpol_grid_spatial_Budapest.RData")
