library(magrittr)

#### research area ####

# load manually crafted research area shape file
research_area <- sf::st_read(
  "data_tracked/research_area/research_area.gpkg", quiet = TRUE
)
save(research_area, file = "data/spatial/research_area.RData")

#### coordinate reference system ####

# ETRS89 Lambert Azimuthal Equal-Area projection "European grid"
# https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2
epsg3035 <- sf::st_crs(research_area)
save(epsg3035, file = "data/spatial/epsg3035.RData")

#### natural earth data ####

# land_outline
# land_outline <- rnaturalearth::ne_download(
#   scale = 50, type = 'land', category = 'physical'
# ) %>% sf::st_as_sf()
# save(land_outline, file = "data_tracked/natural_earth_geodata/land_outline.RData")
load("data_tracked/natural_earth_geodata/land_outline.RData")
save(land_outline, file = "data/spatial/land_outline.RData")

# rivers
# rivers <- rnaturalearth::ne_download(
#   scale = 50, type = 'rivers_lake_centerlines', category = 'physical'
# ) %>% sf::st_as_sf()
# save(rivers, file = "data_tracked/natural_earth_geodata/rivers.RData")
load("data_tracked/natural_earth_geodata/rivers.RData")
save(rivers, file = "data/spatial/rivers.RData")

# lakes
# lakes <- rnaturalearth::ne_download(
#   scale = 50, type = 'lakes', category = 'physical'
# ) %>% sf::st_as_sf()
# save(lakes, file = "data_tracked/natural_earth_geodata/lakes.RData")
load("data_tracked/natural_earth_geodata/lakes.RData")
save(lakes, file = "data/spatial/lakes.RData")

#### area ####

# load natural earth data land outline shape, crop it approximately to
# Europe, transform it to EPSG:3035, crop it to the research area and store the result
land_outline_small <- land_outline %>%
  sf::st_crop(xmin = -20, ymin = 15, xmax = 75, ymax = 65) %>%
  sf::st_transform(epsg3035)
area <- sf::st_intersection(sf::st_buffer(land_outline_small, 0), research_area)
save(area, file = "data/spatial/area.RData")

#### extended area ####

# crop land outline to enlarged bbox of research area
bb <- sf::st_bbox(research_area)
bb[1:2] <- bb[1:2] - 50000
bb[3:4] <- bb[3:4] + 50000
extended_research_area <- bb %>% sf::st_as_sfc()
extended_area <- sf::st_intersection(sf::st_buffer(land_outline_small, 0), extended_research_area)
save(extended_area, file = "data/spatial/extended_area.RData")

#### mobility regions ####

# load manually crafted mobility regions shape file, transform it to
# EPSG:3035 and store the result
mobility_regions <- sf::st_read(
  "data_tracked/mobility_regions/mobility_regions.gpkg", quiet = TRUE
)
mobility_region_names <- c(
  "Iberia",
  "Britain and Ireland",
  "Central Europe",
  "Eastern Balkan"
)
mobility_regions$region_id <- factor(
  mobility_regions$region_id, levels = mobility_region_names
)
save(mobility_regions, file = "data/spatial/mobility_regions.RData")

library(ggplot2)
ggplot() +
  geom_sf(data = extended_area) +
  geom_sf(data = research_area, fill = NA) +
  geom_sf(data = mobility_regions, aes(color = region_id), fill = NA)

