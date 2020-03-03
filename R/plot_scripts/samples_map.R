library(magrittr)
library(ggplot2)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/anno_slices_geo.RData")

ex <- raster::extent(research_area)
xlimit <- c(ex[1] + 100000, ex[2] - 100000)
ylimit <- c(ex[3] + 100000, ex[4] - 100000)

plot <- ggplot() +
  geom_sf(
    data = extended_area,
    fill = "white", colour = "black", size = 0.4
  ) +
  geom_sf(
    data = research_area,
    fill = NA, colour = "red", size = 1
  ) +
  geom_point(
    data = anno,
    aes(x = x, y = y),
    size = 2
  ) +
  theme_bw() +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = sf::st_crs("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
  ) + 
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(colour = "grey", size = 0.3)
  )
  
  ggsave(
    paste0("plots/samples_map.jpeg"),
    plot = plot,
    device = "jpeg",
    scale = 0.5,
    dpi = 300,
    width = 550, height = 300, units = "mm",
    limitsize = F
  )
