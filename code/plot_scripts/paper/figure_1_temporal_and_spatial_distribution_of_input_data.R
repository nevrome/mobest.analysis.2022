library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg102013.RData")
load("data/plot_reference_data/region_id_shapes.RData")
load("data/plot_reference_data/age_colors_gradient.RData")
load("data/spatial/mobility_regions.RData")

ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

# map
p_map <- ggplot() +
  geom_sf(
    data = extended_area,
    fill = "white", colour = "darkgrey", size = 0.4
  ) +
  geom_sf(
    data = mobility_regions,
    fill = NA, colour = "black", size = 0.1
  ) +
  geom_sf(
    data = research_area,
    fill = NA, colour = "black", size = 0.8, linetype = "dashed"
  ) +
  geom_point(
    data = janno_final,
    aes(x = x, y = y, color = Date_BC_AD_Median_Derived, shape = region_id),
    size = 2
  ) +
  theme_bw() +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = sf::st_crs(epsg102013)
  ) + 
  theme(
    axis.title = element_blank(),
    legend.position = "none",
    legend.background = element_blank(),
    legend.title = element_text(size = 13),
    legend.spacing.y = unit(0.2, 'cm'),
    legend.key.height = unit(0.4, 'cm'),
    legend.text = element_text(size = 10),
    panel.background = element_rect(fill = "#BFD5E3"),
    plot.margin = unit(c(5.5, 1, 5.5, 5.5), "points")
  ) +
  age_colors_gradient +
  scale_shape_manual(
    values = region_id_shapes
  ) +
  guides(
    color = guide_colorbar(title = "Time", barwidth = 20, barheight = 1.5),
    shape = guide_legend(title = "Region", nrow = 3, ncol = 4, byrow = F)
  )

# space time plot
p_space_time <- ggplot(
  data = janno_final,
  aes(x = Longitude, y = Date_BC_AD_Median_Derived, color = Date_BC_AD_Median_Derived, shape = region_id)
) +
  geom_point() +
  age_colors_gradient +
  scale_shape_manual(
    values = region_id_shapes
  ) +
  theme_bw() +
  xlab("Longitude") +
  ylab("time in years calBC/AD") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 9),
    plot.margin = unit(c(5.5, 0, 5.5, 0), "points"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) +
  scale_y_continuous(breaks = seq(-8000, 2000, 1000)) +
  coord_cartesian(ylim = c(-7800, 1800))
  
# temporal distribution

p_tempdist <- janno_final %>%
  ggplot() +
  geom_histogram(
    aes(x = Date_BC_AD_Median_Derived),
    fill = "grey",
    breaks = seq(-8000, 2000, 200)
  ) +
  theme_bw() +
  xlab("time in years calBC/AD") +
  ylab("# samples") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "none",
    plot.margin = unit(c(5.5, 5.5, 5.5, 0), "points")
  ) +
  coord_flip(xlim = c(-7800, 1800)) +
  # scale_fill_manual(
  #   values = region_id_colors
  # ) +
  scale_x_continuous(breaks = seq(-8000, 2000, 1000))

# merge plots

right <- cowplot::plot_grid(p_space_time, p_tempdist, ncol = 2, labels = c(NA, NA), align = "h", axis = "lrtb", rel_widths = c(0.75, 0.25))

top <- cowplot::plot_grid(p_map, right, nrow = 1, ncol = 2, rel_widths = c(1, 0.4), labels = c("A", "B"), scale = 0.97)

legend <- cowplot::get_legend(p_map + theme(legend.position = "bottom"))

p <- cowplot::plot_grid(top, legend, nrow = 2, rel_heights = c(0.9, 0.1))

ggsave(
  paste0("plots/figure_1_temporal_and_spatial_distribution_of_input_data.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.5,
  dpi = 300,
  width = 630, height = 300, units = "mm",
  limitsize = F
)

