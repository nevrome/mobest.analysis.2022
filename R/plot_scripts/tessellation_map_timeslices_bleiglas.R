library(magrittr)
library(ggplot2)

load("data/tessellation/tessellation_cut_surfaces_burial_type.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")

ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

#### plot bleiglasfenster plot ####
p <- cut_surfaces_info %>%
  ggplot() +
  geom_sf(
    data = extended_area,
    fill = "white", colour = "black", size = 0.4
  ) +
  geom_sf(
    aes(fill = PC1), 
    color = "white", lwd = 0.3
  ) +
  geom_sf(
    data = research_area,
    fill = NA, colour = "red", size = 0.6
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    breaks = round(seq(min(cut_surfaces_info[["PC1"]]), max(cut_surfaces_info[["PC1"]]), 0.03), 2)
  ) +
  facet_wrap(~time, nrow = 3) +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = sf::st_crs("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
  ) +
  guides(
    fill = guide_colorbar(barwidth = 50)
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 20, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(size = 20),
    panel.grid.major = element_line(colour = "grey", size = 0.3),
    strip.text.x = element_text(size = 20)
  )

p %>%
  ggsave(
    "plots/bleiglas_map_PC1.png",
    plot = .,
    device = "png",
    scale = 1,
    dpi = 300,
    width = 550, height = 260, units = "mm",
    limitsize = F
  )

