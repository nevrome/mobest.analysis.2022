library(magrittr)

epsg102013 <- paste(
  "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30",
  "+lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs"
)
save(epsg102013, file = "data/spatial/epsg102013.RData")

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

#### research area ####

# load manually crafted research area shape file, transform it to
# EPSG:102013 and store the result
research_area <- sf::st_read(
  "data_tracked/research_area/research_area.shp", quiet = TRUE
) %>% sf::st_transform(epsg102013)
save(research_area, file = "data/spatial/research_area.RData")

#### area ####

# load natural earth data land outline shape, crop it approximately to
# Europe, transform it to EPSG:102013, crop it to the research area and store the result
land_outline_small <- land_outline %>%
  sf::st_crop(xmin = -20, ymin = 15, xmax = 75, ymax = 65) %>%
  sf::st_transform(epsg102013)
area <- sf::st_intersection(sf::st_buffer(land_outline_small, 0), research_area)
save(area, file = "data/spatial/area.RData")

#### extended area ####

# crop land outline to enlarged bbox of research area
bb <- sf::st_bbox(research_area)
bb[1:2] <- bb[1:2] - 500000
bb[3:4] <- bb[3:4] + 500000
extended_research_area <- bb %>% sf::st_as_sfc()
extended_area <- sf::st_intersection(sf::st_buffer(land_outline_small, 0), extended_research_area)
save(extended_area, file = "data/spatial/extended_area.RData")

#### mobility regions ####

# load manually crafted mobility regions shape file, transform it to
# EPSG:102013 and store the result
mobility_regions <- sf::st_read(
  "data_tracked/mobility_regions/mobility_regions.shp", quiet = TRUE
) %>% sf::st_transform(epsg102013)
mobility_regions$region_id <- factor(
  mobility_regions$region_id, levels = c(
    "Britain and Ireland",
    "Southern Scandinavia",
    "Baltics",
    "Eastern Europe",
    "France",
    "Central Europe",
    "Southeastern Europe",
    "Caucasus",
    "Iberia",
    "Italy",
    "Turkey",
    "Levant"
  )
)
save(mobility_regions, file = "data/spatial/mobility_regions.RData")
