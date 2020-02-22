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

#### prep independent variables with temporal sampling ####

independent_list <- lapply(
  1:30,#1:length(anno$calage_sample[[1]]), 
  function(i, anno) {
    age_sample <- sapply(anno$calage_sample, function(x){ x[i] })
    dplyr::transmute(
      .data = anno,
      x_01 = range_01_x(x),
      y_01 = range_01_y(y),
      z_01 = range_01_z(age_sample)
    )
  },
  anno
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

pred_grid <- pred_points_space %>% 
  tidyr::crossing(time_layers) %>%
  dplyr::mutate(
    x_01 = range_01_x(x_real),
    y_01 = range_01_y(y_real),
    z_01 = range_01_z(age_sample)
  )

save.image(file = "data/gpr/gpr_temporal_sampling.RData", version = 2)

# # transform pred grid to spatial object
# pred_grid_spatial_cropped <- sf::st_as_sf(pred_grid, coords = c("x_real", "y_real"), crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs") %>%
#  dplyr::mutate(
#    x_real = sf::st_coordinates(.)[,1],
#    y_real = sf::st_coordinates(.)[,2]
#  )

# #### store results ####
# save(pred_grid_spatial_cropped, file = "data/gpr/pred_grid_spatial_cropped_temporal_sampling.RData")


