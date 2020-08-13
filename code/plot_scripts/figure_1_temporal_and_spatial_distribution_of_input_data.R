library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg102013.RData")
load("data/plot_reference_data/region_id_colors.RData")
load("data/plot_reference_data/age_group_id_shapes.RData")

ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

# map
p_map <- ggplot() +
  geom_sf(
    data = extended_area,
    fill = "white", colour = "black", size = 0.4
  ) +
  geom_sf(
    data = research_area,
    fill = NA, colour = "black", size = 0.8, linetype = "dashed"
  ) +
  geom_point(
    data = janno_final,
    aes(x = x, y = y, color = region_id, shape = age_group_id),
    size = 2
  ) +
  theme_bw() +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = sf::st_crs(epsg102013)
  ) + 
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 10),
    legend.position = "none",
    legend.background = element_blank(),
    legend.title = element_text(size = 13),
    legend.spacing.x = unit(0.2, 'cm'),
    legend.key.height = unit(0.4, 'cm'),
    legend.text = element_text(size = 9),
    panel.background = element_rect(fill = "#BFD5E3")
  ) +
  scale_color_manual(
    values = region_id_colors
  ) +
  scale_shape_manual(
    values = age_group_id_shapes
  ) +
  guides(
    color = guide_legend(title = "Region", nrow = 3, ncol = 4),
    shape = guide_legend(title = "Time", nrow = 3, ncol = 4)
  )

# space time plot
p_space_time <- ggplot(
  data = janno_final,
  aes(x = Longitude, y = Date_BC_AD_Median_Derived, color = region_id, shape = age_group_id)
) +
  geom_point() +
  scale_shape_manual(
    values = age_group_id_shapes,
    guide = FALSE
  ) +
  scale_color_manual(
    values = region_id_colors,
    guide = FALSE
  ) +
  theme_bw() +
  xlab("Longitude") +
  ylab("time in years calBC/AD") +
  theme(
    axis.title = element_text(size = 9)
  )
  
# temporal distribution

p_tempdist <- janno_final %>%
  ggplot() +
  geom_histogram(
    aes(x = Date_BC_AD_Median_Derived), binwidth = 100
  ) +
  theme_bw() +
  xlab("time in years calBC/AD") +
  ylab("number of samples per century") +
  theme(
    axis.title = element_text(size = 9)
  ) +
  coord_flip()

# merge plots

right <- cowplot::plot_grid(p_space_time, p_tempdist, ncol = 1, labels = c("B", "C"), hjust = 0.6, vjust = 0.8)

top <- cowplot::plot_grid(p_map, right, nrow = 1, ncol = 2, rel_widths = c(1, 0.4), labels = c("A", NA), scale = 0.97)

legend <- cowplot::get_legend(p_map + theme(legend.position = "bottom"))

p <- cowplot::plot_grid(top, legend, nrow = 2, rel_heights = c(1, 0.1))

ggsave(
  paste0("plots/figure_1_temporal_and_spatial_distribution_of_input_data.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.5,
  dpi = 300,
  width = 630, height = 300, units = "mm",
  limitsize = F
)

