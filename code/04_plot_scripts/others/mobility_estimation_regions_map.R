library(magrittr)
library(ggplot2)

load("data/spatial/mobility_regions.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")

ex <- raster::extent(research_area)
xlimit <- c(ex[1] + 100000, ex[2] - 100000)
ylimit <- c(ex[3] + 100000, ex[4] - 100000)

plot <- ggplot() +
  geom_sf(
    data = extended_area,
    fill = "white", colour = "black", size = 0.4
  ) +
  geom_sf(
    data = mobility_regions,
    fill = "blue",
    color = "blue",
    alpha = 0.1
  ) +
  geom_sf_label(
    data = mobility_regions,
    aes(label = region_id),
    color = "black",
    size = 4.7
  ) +
  theme_bw() +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = sf::st_crs("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
  ) + 
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 15),
    panel.grid.major = element_line(colour = "grey", size = 0.3)
  )

ggsave(
  paste0("plots/mobility_regions_map.jpeg"),
  plot = plot,
  device = "jpeg",
  scale = 0.5,
  dpi = 300,
  width = 550, height = 300, units = "mm",
  limitsize = F
)
