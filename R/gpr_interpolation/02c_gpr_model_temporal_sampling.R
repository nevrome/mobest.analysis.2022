load("/projects1/coest_mobility/coest.interpol.2020/data/gpr/pred_grid_temporal_sampling.RData")

# transform pred grid to spatial object
pred_grid_spatial_cropped <- sf::st_as_sf(pred_grid, coords = c("x_real", "y_real"), crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs") %>%
  dplyr::mutate(
    x_real = sf::st_coordinates(.)[,1],
    y_real = sf::st_coordinates(.)[,2]
  )

#### store results ####
save(pred_grid_spatial_cropped, file = "data/gpr/pred_grid_spatial_cropped_temporal_sampling.RData")


