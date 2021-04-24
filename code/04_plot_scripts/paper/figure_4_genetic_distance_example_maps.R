library(magrittr)
library(ggplot2)

load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
load("data/origin_search/janno_search.RData")
load("data/origin_search/distance_grid_examples.RData")

p <- ggplot() +
  facet_wrap(~z) +
  geom_sf(data = extended_area, fill = "black") +
  geom_raster(
    data = dist_grid,
    mapping = aes(x = x, y = y, fill = gen_dist),
  ) +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  geom_sf(data = extended_area, fill = NA, colour = "black") +
  geom_point(
    data = janno_search,
    mapping = aes(x = x, y = y),
    colour = "red",
    size = 4
  ) +
  geom_point(
    data = dist_grid %>% dplyr::filter(min_gen_dist),
    mapping = aes(x = x, y = y),
    colour = "red",
    shape = "✖",
    size = 5
  ) +
  theme_bw() +
  coord_sf(
    expand = FALSE,
    crs = epsg3035
  ) +
  guides(
    fill = guide_colorbar(title = "Genetic distance  ", barwidth = 25)
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 12),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 15),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#BFD5E3")
  )

ggsave(
  "plots/figure_4_genetic_distance_example_maps.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.5,
  dpi = 300,
  width = 800, height = 300, units = "mm",
  limitsize = F
)

