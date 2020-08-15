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
  coord_fixed(xlim = c(-0.05, 0.09), ylim = c(-0.07, 0.065)) +
  scale_y_continuous(breaks = seq(-0.08, 0.08, 0.02)) +
  scale_x_continuous(breaks = seq(-0.06, 0.08, 0.02))

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
    size = 1
  ) +
  geom_errorbar(
    data = poi_timeseries_Budapest,
    aes(
      x = mean_C1, 
      ymin = mean_C2 - sd_C2, ymax = mean_C2 + sd_C2,
      color = z
    ),
    size = 0.7, alpha = 0.5
  ) +
  geom_errorbarh(
    data = poi_timeseries_Budapest,
    aes(
      y = mean_C2, 
      xmin = mean_C1 - sd_C1, xmax = mean_C1 + sd_C1,
      color = z
    ),
    size = 0.7, alpha = 0.5
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
    limits = c(-8000, 1000), low = "black", mid = "red", high = "green", midpoint = -5000,
    breaks = seq(-8000, 1000, 3000)
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  coord_fixed(xlim = c(-0.05, 0.09), ylim = c(-0.07, 0.065)) +
  scale_y_continuous(breaks = seq(-0.08, 0.08, 0.02)) +
  scale_x_continuous(breaks = seq(-0.06, 0.08, 0.04))

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
    size = 1
  ) +
  geom_errorbar(
    data = poi_timeseries_London,
    aes(
      x = mean_C1, 
      ymin = mean_C2 - sd_C2, ymax = mean_C2 + sd_C2,
      color = z
    ),
    size = 0.7, alpha = 0.5
  ) +
  geom_errorbarh(
    data = poi_timeseries_London,
    aes(
      y = mean_C2, 
      xmin = mean_C1 - sd_C1, xmax = mean_C1 + sd_C1,
      color = z
    ),
    size = 0.7, alpha = 0.5
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
    limits = c(-8000, 1000), low = "black", mid = "red", high = "green", midpoint = -5000,
    breaks = seq(-8000, 1000, 1000)
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.title = element_text(size = 13),
    legend.spacing.x = unit(0.2, 'cm'),
    legend.key.height = unit(0.4, 'cm'),
    legend.text = element_text(size = 9)
  ) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  coord_fixed(xlim = c(-0.05, 0.09), ylim = c(-0.07, 0.065)) +
  scale_y_continuous(breaks = seq(-0.08, 0.08, 0.02)) +
  scale_x_continuous(breaks = seq(-0.06, 0.08, 0.04)) +
  guides(
    color = guide_legend(title = "Prediction time", nrow = 1)
  )

# merge plots

right <- cowplot::plot_grid(p_Budapest, p_London + theme(legend.position = "none"), ncol = 1, labels = c("B", "C"))

top <- cowplot::plot_grid(p_mds, right, nrow = 1, rel_widths = c(1, 0.5), labels = "A")

p <- cowplot::plot_grid(top, cowplot::get_legend(p_London), rel_heights = c(1, 0.1), ncol = 1)

ggsave(
  paste0("plots/figure_4_mds.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.5,
  dpi = 300,
  width = 440, height = 300, units = "mm",
  limitsize = F
)

