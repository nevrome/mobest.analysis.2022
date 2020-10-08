library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")
load("data/plot_reference_data/region_id_colors.RData")
load("data/plot_reference_data/age_group_id_shapes.RData")
load("data/gpr/interpol_grid_examples.RData")

poi_timeseries <- interpol_grid_examples %>%
  tidyr::pivot_wider(
    id_cols = c("z", "kernel_setting_id", "pred_grid_id"),
    names_from = "dependent_var_id",
    values_from = c("mean", "sd")
  )

poi_timeseries_Budapest <- poi_timeseries %>% dplyr::filter(pred_grid_id == "Budapest")
poi_timeseries_London <- poi_timeseries %>% dplyr::filter(pred_grid_id == "London")
poi_timeseries_Rome <- poi_timeseries %>% dplyr::filter(pred_grid_id == "Rome")
poi_timeseries_Jerusalem <- poi_timeseries %>% dplyr::filter(pred_grid_id == "Jerusalem")

# normal mds plot
p_mds <- ggplot() +
  geom_point(
    data = janno_final,
    aes(x = C1, y = C2, color = region_id, shape = age_group_id),
    alpha = 0.7,
    size = 2
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  scale_shape_manual(
    values = age_group_id_shapes
  ) +
  scale_color_manual(
    values = region_id_colors
  ) +
  guides(
    color = guide_legend(title = ""),
    shape = guide_legend(title = "median age calBC")
  ) +
  coord_fixed(xlim = c(-0.1, 0.05), ylim = c(-0.07, 0.065)) +
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  scale_x_continuous(breaks = seq(-0.1, 0.1, 0.02))

# London
p_London <- ggplot() +
  geom_point(
    data = janno_final,
    aes(x = C1, y = C2),
    alpha = 0.1, size = 1, shape = 3
  ) +  
  geom_path(
    data = poi_timeseries_London,
    aes(x = mean_C1, y = mean_C2),
    size = 0.8
  ) +
  geom_errorbar(
    data = poi_timeseries_London,
    aes(
      x = mean_C1, 
      ymin = mean_C2 - sd_C2, ymax = mean_C2 + sd_C2,
      color = z
    ),
    size = 0.4,
  ) +
  geom_errorbarh(
    data = poi_timeseries_London,
    aes(
      y = mean_C2, 
      xmin = mean_C1 - sd_C1, xmax = mean_C1 + sd_C1,
      color = z
    ),
    size = 0.4,
  ) +
  geom_point(
    data = poi_timeseries_London,
    aes(
      x = mean_C1, 
      y = mean_C2,
      color = z
    ),
    size = 2
  ) +
  scale_color_gradient2(
    limits = c(-7500, 1500), low = "black", mid = "#fc8d62", high = "#66c2a5", midpoint = -5000,
    breaks = seq(-7500, 1500, 1000)
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.title = element_text(size = 13),
    legend.spacing.x = unit(0.2, 'cm'),
    legend.key.height = unit(0.4, 'cm'),
    legend.text = element_text(size = 9),
    axis.title = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  coord_fixed(xlim = c(-0.1, 0.05), ylim = c(-0.07, 0.065)) +
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  scale_x_continuous(breaks = seq(-0.08, 0.1, 0.04)) +
  guides(
    color = guide_legend(title = "Prediction time", nrow = 1)
  ) +
  ggtitle("London")

# Budapest
p_Budapest <- ggplot() +
  geom_point(
    data = janno_final,
    aes(x = C1, y = C2),
    alpha = 0.1, size = 1, shape = 3
  ) +  
  geom_path(
    data = poi_timeseries_Budapest,
    aes(x = mean_C1, y = mean_C2),
    size = 0.8
  ) +
  geom_errorbar(
    data = poi_timeseries_Budapest,
    aes(
      x = mean_C1, 
      ymin = mean_C2 - sd_C2, ymax = mean_C2 + sd_C2,
      color = z
    ),
    size = 0.4,
  ) +
  geom_errorbarh(
    data = poi_timeseries_Budapest,
    aes(
      y = mean_C2, 
      xmin = mean_C1 - sd_C1, xmax = mean_C1 + sd_C1,
      color = z
    ),
    size = 0.4,
  ) +
  geom_point(
    data = poi_timeseries_Budapest,
    aes(
      x = mean_C1, 
      y = mean_C2,
      color = z
    ),
    size = 2
  ) +
  scale_color_gradient2(
    limits = c(-7500, 1500), low = "black", mid = "#fc8d62", high = "#66c2a5", midpoint = -5000,
    breaks = seq(-7500, 1500, 1000)
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    legend.position = "none",
    axis.title = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  coord_fixed(xlim = c(-0.1, 0.05), ylim = c(-0.07, 0.065)) +
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  scale_x_continuous(breaks = seq(-0.08, 0.1, 0.04)) +
  ggtitle("Budapest")

# Rome
p_Rome <- ggplot() +
  geom_point(
    data = janno_final,
    aes(x = C1, y = C2),
    alpha = 0.1, size = 1, shape = 3
  ) +  
  geom_path(
    data = poi_timeseries_Rome,
    aes(x = mean_C1, y = mean_C2),
    size = 0.8
  ) +
  geom_errorbar(
    data = poi_timeseries_Rome,
    aes(
      x = mean_C1, 
      ymin = mean_C2 - sd_C2, ymax = mean_C2 + sd_C2,
      color = z
    ),
    size = 0.4,
  ) +
  geom_errorbarh(
    data = poi_timeseries_Rome,
    aes(
      y = mean_C2, 
      xmin = mean_C1 - sd_C1, xmax = mean_C1 + sd_C1,
      color = z
    ),
    size = 0.4,
  ) +
  geom_point(
    data = poi_timeseries_Rome,
    aes(
      x = mean_C1, 
      y = mean_C2,
      color = z
    ),
    size = 2
  ) +
  scale_color_gradient2(
    limits = c(-7500, 1500), low = "black", mid = "#fc8d62", high = "#66c2a5", midpoint = -5000,
    breaks = seq(-7500, 1500, 1000)
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    legend.position = "none",
    axis.title = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  coord_fixed(xlim = c(-0.1, 0.05), ylim = c(-0.07, 0.065)) +
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  scale_x_continuous(breaks = seq(-0.08, 0.1, 0.04)) +
  ggtitle("Rome")

# Jerusalem
p_Jerusalem <- ggplot() +
  geom_point(
    data = janno_final,
    aes(x = C1, y = C2),
    alpha = 0.1, size = 1, shape = 3
  ) +  
  geom_path(
    data = poi_timeseries_Jerusalem,
    aes(x = mean_C1, y = mean_C2),
    size = 0.8
  ) +
  geom_errorbar(
    data = poi_timeseries_Jerusalem,
    aes(
      x = mean_C1, 
      ymin = mean_C2 - sd_C2, ymax = mean_C2 + sd_C2,
      color = z
    ),
    size = 0.4
  ) +
  geom_errorbarh(
    data = poi_timeseries_Jerusalem,
    aes(
      y = mean_C2, 
      xmin = mean_C1 - sd_C1, xmax = mean_C1 + sd_C1,
      color = z
    ),
    size = 0.4
  ) +
  geom_point(
    data = poi_timeseries_Jerusalem,
    aes(
      x = mean_C1, 
      y = mean_C2,
      color = z
    ),
    size = 2
  ) +
  scale_color_gradient2(
    limits = c(-7500, 1500), low = "black", mid = "#fc8d62", high = "#66c2a5", midpoint = -5000,
    breaks = seq(-7500, 1500, 1000)
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    legend.position = "none",
    axis.title = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  coord_fixed(xlim = c(-0.1, 0.05), ylim = c(-0.07, 0.065)) +
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  scale_x_continuous(breaks = seq(-0.08, 0.1, 0.04)) +
  ggtitle("Jerusalem")

# merge plots

left <- cowplot::plot_grid(p_London + theme(legend.position = "none"), p_Rome, ncol = 1, labels = c("B", "D"))

right <- cowplot::plot_grid(p_Budapest, p_Jerusalem, ncol = 1, labels = c("C", "E"))

top <- cowplot::plot_grid(p_mds, left, right, nrow = 1, rel_widths = c(1.2, 0.5, 0.5), labels = c("A", NA, NA))

p <- cowplot::plot_grid(top, cowplot::get_legend(p_London), rel_heights = c(1, 0.1), ncol = 1)

ggsave(
  paste0("plots/figure_4_mds.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.5,
  dpi = 300,
  width = 570, height = 300, units = "mm",
  limitsize = F
)

