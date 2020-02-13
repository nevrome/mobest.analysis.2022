library(magrittr)
library(laGP)
source("R/helper_functions.R")

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/area.RData")
bb <- unname(sf::st_bbox(research_area))

#### prep independent variables ####

independent <- anno %>%
  dplyr::transmute(
    x_01 = range_01_x(x),
    y_01 = range_01_y(y),
    z_01 = range_01_z(calage_center)
  )

#### create prediction grid ####

pred_points_space <- area %>% 
  sf::st_make_grid(cellsize = 100000, what = "centers") %>%
  sf::st_intersection(area) %>%
  sf::st_coordinates() %>%
  tibble::as_tibble() %>%
  dplyr::rename(x_real = X, y_real = Y)

time_layers <- tibble::tibble(
  age_sample = seq(-7500, -500, 500)
)

pred_grid <- pred_grid <- pred_points_space %>% 
  tidyr::crossing(time_layers) %>%
  dplyr::mutate(
    x_01 = range_01_x(x_real),
    y_01 = range_01_y(y_real),
    z_01 = range_01_z(age_sample)
  )

#### gp regression ####

gp_PC1 <- newGPsep(
  X = independent, 
  Z = anno$PC1, 
  d = c(dist_scale_01_x_km(50), dist_scale_01_y_km(50), dist_scale_01_z_y(200)), 
  g = 0.2,
  dK = TRUE
)
pred_PC1 <- predGPsep(gp_PC1, XX = pred_grid[, c("x_01", "y_01", "z_01")], lite = T)
deleteGPsep(gp_PC1)

gp_PC2 <- newGPsep(
  X = independent,
  Z = anno$PC2,
  d = c(dist_scale_01_x_km(50), dist_scale_01_y_km(50), dist_scale_01_z_y(200)),
  g = 0.2,
  dK = TRUE
)
pred_PC2 <- predGPsep(gp_PC2, XX = pred_grid[, c("x_01", "y_01", "z_01")], lite = T)
deleteGPsep(gp_PC2)

#### store prediction results #### 

pred_grid$pred_PC1_mean <- pred_PC1$mean
pred_grid$pred_PC1_s2 <- pred_PC1$s2
pred_grid$pred_PC1_sd <- sqrt(pred_PC1$s2)
pred_grid$pred_PC2_mean <- pred_PC2$mean
pred_grid$pred_PC2_s2 <- pred_PC2$s2
pred_grid$pred_PC2_sd <- sqrt(pred_PC2$s2)

#### spatially crop prediction grid ####

pred_grid_spatial_cropped <- sf::st_as_sf(pred_grid, coords = c("x_real", "y_real"), crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs") %>%
  #sf::st_intersection(research_area) %>%
  dplyr::mutate(
    x_real = sf::st_coordinates(.)[,1],
    y_real = sf::st_coordinates(.)[,2]
  )

#### store results ####
save(pred_grid_spatial_cropped, file = "data/gpr/pred_grid_spatial_cropped.RData")
