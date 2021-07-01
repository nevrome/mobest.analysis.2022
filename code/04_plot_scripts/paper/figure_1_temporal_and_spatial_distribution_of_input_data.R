library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
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
    data = research_area,
    fill = NA, colour = "black", size = 0.8, linetype = "dashed"
  ) +
  geom_jitter(
    data = janno_final %>% dplyr::arrange(Date_BC_AD_Median_Derived),
    aes(x = x, y = y, color = Date_BC_AD_Median_Derived, shape = region_id),
    size = 1.5,
    alpha = 1,
    width = 60000,
    height = 60000
  ) +
  # geom_sf_label(
  #   data = mobility_regions,
  #   aes(label = region_id),
  #   colour = "black", size = 4,
  #   alpha = 0.3
  # ) +
  theme_bw() +
  coord_sf(
    expand = FALSE,
    crs = sf::st_crs(epsg3035)
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
    values = region_id_shapes,
    na.value = 3
  ) +
  guides(
    color = guide_colorbar(title = "Time", barwidth = 20, barheight = 1.5),
    shape = guide_legend(
      title = "Region", nrow = 3, ncol = 3, byrow = T,
      override.aes = aes(size = 3)
    )
  )

# space time plot
p_space_time <- ggplot(
  data = janno_final %>% dplyr::filter(!is.na(region_id)),
  aes(
    x = region_id, y = Date_BC_AD_Median_Derived, 
    color = Date_BC_AD_Median_Derived, 
    shape = region_id
  )
) +
  geom_jitter(height = 0, width = 0.3) +
  age_colors_gradient +
  scale_shape_manual(
    values = region_id_shapes,
    na.value = 3
  ) +
  theme_bw() +
  xlab("") +
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
    aes(x = Date_BC_AD_Median_Derived, fill = !is.na(region_id)),
    breaks = seq(-8000, 2000, 200)
  ) +
  scale_fill_manual(
    values = c("TRUE" = "black", "FALSE" = "grey")
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
  scale_x_continuous(breaks = seq(-8000, 2000, 1000)) +
  scale_y_continuous(breaks = c(0, 100, 200))

# merge plots

right <- cowplot::plot_grid(p_space_time, p_tempdist, ncol = 2, labels = c(NA, NA), align = "h", axis = "lrtb", rel_widths = c(0.75, 0.25))

top <- cowplot::plot_grid(p_map, right, nrow = 1, ncol = 2, rel_widths = c(1, 0.4), labels = c("A", "B"), scale = 0.97)

legend <- cowplot::get_legend(p_map + theme(legend.position = "bottom"))

p <- cowplot::plot_grid(top, legend, nrow = 2, rel_heights = c(0.9, 0.1))

ggsave(
  paste0("plots/figure_1_temporal_and_spatial_distribution_of_input_data.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 530, height = 300, units = "mm",
  limitsize = F
)

