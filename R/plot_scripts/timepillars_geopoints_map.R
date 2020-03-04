library(magrittr)
library(ggplot2)

load("data/gpr/pred_grid_filled_grouped_spatial.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")

# dots on the map
poi <- tibble::tribble(
  ~poi_id, ~lat, ~lon,
  "Madrid", 40.4, -3.6,
  "Berlin", 52.5, 13.4,
  "Bucharest", 44.4, 26.1,
) %>% sf::st_as_sf(
  coords = c("lon", "lat"),
  crs = 4326
) %>%
  sf::st_transform("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")

# plot map 
ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

plot_map <- ggplot() +
  geom_sf(
    data = extended_area,
    fill = "white", colour = "black", size = 0.4
  ) +
  geom_sf(
    data = poi,
    color = "red",
    shape = 4,
    size = 5
  ) +
  geom_sf_text(
    data = poi,
    aes(label = poi_id),
    color = "red",
    size = 7,
    nudge_y = -150000
  ) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 15),
    panel.grid.major = element_line(colour = "grey", size = 0.3),
  ) +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = sf::st_crs("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
  )

ggsave(
  "plots/timepillars_geopoints_map.jpeg",
  plot = plot_map,
  device = "jpeg",
  scale = 0.5,
  dpi = 300,
  width = 550, height = 300, units = "mm",
  limitsize = F
)
