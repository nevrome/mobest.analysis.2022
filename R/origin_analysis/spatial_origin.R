library(magrittr)

load("data/gpr/interpol_grid.RData")

pri_ready <- mobest::search_spatial_origin(interpol_grid)

save(pri_ready, file = "data/pri_ready.RData")

# spatialize
pri_ready_spatial <- sf::st_as_sf(
  pri_ready, 
  coords = c("x_real", "y_real"), 
  crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
  remove = F
)

save(pri_ready_spatial, file = "data/pri_ready_spatial.RData")

