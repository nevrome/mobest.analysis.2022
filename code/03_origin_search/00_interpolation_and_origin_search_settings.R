default_kernel <- mobest::create_kernset_multi(
  d = list(c(700000, 700000, 800)), 
  g = 0.06, 
  on_residuals = T, 
  auto = F,
  it = "ds700_dt800_g006"
)

save(default_kernel, file = "data/origin_search/default_kernel.RData")

library(sf)
load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")
load("data/spatial/epsg3035.RData")

search_area <- janno_final %>%
  dplyr::select(Individual_ID, x, y) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = epsg3035) %>%
  sf::st_buffer(dist = 200000) %>%
  sf::st_union() %>%
  sf::st_intersection(
    area
  )

save(search_area, file = "data/origin_search/search_area.RData")


