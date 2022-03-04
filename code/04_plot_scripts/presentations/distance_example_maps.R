library(magrittr)
library(ggplot2)

load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
load("data/origin_search/janno_search.RData")
load("data/origin_search/closest_points_examples.RData")
load("data/origin_search/distance_grid_examples.RData")

janno_search %<>% dplyr::filter(Poseidon_ID == "3DT26.SG")
closest_points_examples %<>% dplyr::filter(Poseidon_ID == "3DT26.SG")
distance_grid_examples %<>% dplyr::filter(Poseidon_ID == "3DT26.SG")

p <- ggplot() +
  geom_sf(data = extended_area, fill = "black") +
  geom_raster(
    data = distance_grid_examples %>%
      dplyr::group_by(Poseidon_ID, x, y, z) %>%
      dplyr::summarise(
        gen_dist = mean(gen_dist),
        .groups = "drop"
      ),
    mapping = aes(x = x, y = y, fill = gen_dist),
  ) +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  geom_sf(data = extended_area, fill = NA, colour = "black") +
  geom_point(
    data = janno_search,
    mapping = aes(x = x, y = y),
    colour = "red",
    size = 7
  ) +
  geom_point(
    data = closest_points_examples,
    mapping = aes(x = x, y = y),
    colour = "orange",
    size = 7,
    shape = "✖"
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
    legend.title = element_text(size = 15),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(size = 15),
    strip.text = element_text(size = 15),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#BFD5E3")
  )

ggsave(
  "plots/presentation/origin_search_distance_example_map.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 400, height = 300, units = "mm",
  limitsize = F
)

