library(magrittr)

#### load data ####

load("data/gpr/model_grid_simplified.RData")

#### unnest prediction to get a point-wise prediction table ####

pred_grid_filled <- mobest::unnest_model_grid(model_grid_simplified)

#### store result ####

save(pred_grid_filled, file = "data/gpr/pred_grid_filled.RData")

#### transform pred grid to spatial object ####

pred_grid_filled_spatial <- sf::st_as_sf(
  pred_grid_filled, 
  coords = c("x", "y"), 
  crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
  remove = FALSE
)

#### store results ####

save(pred_grid_filled_spatial, file = "data/gpr/pred_grid_filled_spatial.RData")

#### group all age_sampling runs in pred_grid_filled #### 

pred_grid_filled_grouped <- mobest::condense_interpol_grid(pred_grid_filled)

#### store result ####

save(pred_grid_filled_grouped, file = "data/gpr/pred_grid_filled_grouped.RData")

#### transform pred grid to spatial object ####

pred_grid_filled_grouped_spatial <- sf::st_as_sf(
  pred_grid_filled_grouped, 
  coords = c("x", "y"), 
  crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
  remove = FALSE
)

#### store results ####

save(pred_grid_filled_grouped_spatial, file = "data/gpr/pred_grid_filled_grouped_spatial.RData")
