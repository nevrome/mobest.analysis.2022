library(magrittr)
library(ggplot2)

load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg102013.RData")
load("data/mobility_estimation/mobility_proxy_median.RData")
load("data/plot_reference_data/region_id_colors.RData")
load("data/spatial/mobility_regions.RData")

ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

p_arrows <- ggplot() +
  geom_sf(data = extended_area, fill = "white") +
  facet_wrap(dplyr::vars(z), ncol = 2) +
  geom_sf(data = extended_area, fill = NA, colour = "black") +
  geom_sf(
    data = research_area, fill = NA, colour = "black", linetype = "dashed",
    size = 0.8
  ) +
  # geom_segment(
  #   data = mobility_proxy %>% dplyr::filter(z %in% seq(-7500, 1500, 1000), !is.na(region_id)),
  #   aes(
  #     x = x, y = y, xend = x_origin, yend = y_origin,
  #     color = region_id
  #   ),
  #   alpha = 0.5,
  #   size = 0.3,
  #   lineend = "round",
  #   linejoin = "bevel"
  # ) +
  geom_point(
    data = mobility_proxy %>% dplyr::filter(z %in% seq(-7500, 1500, 1000), !is.na(region_id)),
    aes(x = x, y = y, color = region_id),
    alpha = 1,
    size = 1.4,
    shape = 0
  ) +
  geom_point(
    data = mobility_proxy %>% dplyr::filter(z %in% seq(-7500, 1500, 1000), !is.na(region_id)),
    aes(x = x_origin, y = y_origin, color = region_id),
    alpha = 0.7,
    size = 1.2,
    shape = 15
  ) +
  geom_sf(
    data = mobility_regions,
    fill = NA, colour = "black", size = 0.3
  ) +
  theme_bw() +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = epsg102013
  ) +
  scale_color_manual(
    values = region_id_colors
  ) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    strip.text = element_text(size = 12),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#BFD5E3")
  )

ggsave(
  paste0("plots/figure_sup_18_arrow_map_matrix.jpeg"),
  plot = p_arrows,
  device = "jpg",
  scale = 0.5,
  dpi = 300,
  width = 500, height = 780, units = "mm",
  limitsize = F
)
