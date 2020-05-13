library(magrittr)

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final
load("data/spatial/area.RData")
load("data/spatial/mobility_regions.RData")

# individual point
Berlin <- sf::st_as_sf(
  tibble::tibble(lon = 19.04, lat = 47.50),
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
) %>%
  sf::st_transform(
    crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
  ) %>% sf::st_coordinates()

#### prepare pca model grid ####
model_grid_pca <- mobest::create_model_grid(
  independent = c(
    list(age_center = tibble::tibble(x = anno$x, y = anno$y, z = anno$calage_center))
  ),
  dependent = list(
    PC1 = anno$PC1,
    PC2 = anno$PC2
  ),
  kernel = list(
    ds200_dt200_g001 = list(d = c(200000, 200000, 200), g = 0.01, on_residuals = T, auto = F),
    ds500_dt500_g001 = list(d = c(500000, 500000, 500), g = 0.01, on_residuals = T, auto = F),
    ds1000_dt1000_g001 = list(d = c(1000000, 1000000, 1000), g = 0.01, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    Berlin = tibble::tibble(x = Berlin[1], y = Berlin[2], z = seq(-7500, -500, 100), point_id = 1:71)
  )
)

#### prepare mds model grid ####

anno_mds <- anno %>% dplyr::filter(
  !is.na(C1)
)

model_grid_mds <- mobest::create_model_grid(
  independent = c(
    list(age_center = tibble::tibble(x = anno_mds$x, y = anno_mds$y, z = anno_mds$calage_center))
  ),
  dependent = list(
    C1 = anno_mds$C1,
    C2 = anno_mds$C2
  ),
  kernel = list(
    ds200_dt200_g001 = list(d = c(200000, 200000, 200), g = 0.01, on_residuals = T, auto = F),
    ds500_dt500_g001 = list(d = c(500000, 500000, 500), g = 0.01, on_residuals = T, auto = F),
    ds1000_dt1000_g001 = list(d = c(1000000, 1000000, 1000), g = 0.01, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    Berlin = tibble::tibble(x = Berlin[1], y = Berlin[2], z = seq(-7500, -500, 100), point_id = 1:71)
  )
)

#### merge model grids ####

model_grid <- rbind(model_grid_pca, model_grid_mds)

#### run interpolation on model grid ####

model_grid_result <- mobest::run_model_grid(model_grid)

#### unnest prediction to get a point-wise prediction table ####

interpol_grid <- mobest::unnest_model_grid(model_grid_result)

save(interpol_grid, file = "data/gpr/interpol_grid_Berlin.RData")

interpol_grid_spatial <- sf::st_as_sf(
  interpol_grid,
  coords = c("x", "y"),
  crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
  remove = FALSE
)
save(interpol_grid_spatial, file = "data/gpr/interpol_grid_spatial_Berlin.RData")
