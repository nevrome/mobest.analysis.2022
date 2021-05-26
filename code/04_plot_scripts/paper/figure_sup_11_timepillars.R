library(magrittr)
library(ggplot2)

load("data/gpr/interpol_grid_examples.RData")
load("data/plot_reference_data/age_colors_gradient.RData")

poi_timeseries <- interpol_grid_examples %>%
  tidyr::pivot_wider(
    id_cols = c("z", "kernel_setting_id", "pred_grid_id"),
    names_from = "dependent_var_id",
    values_from = c("mean", "sd")
  )

p <- ggplot() +
  facet_wrap(~pred_grid_id) +
  geom_point(
    data = janno_final,
    aes(x = C1, y = C2),
    alpha = 0.1, size = 1, shape = 3
  ) +  
  geom_path(
    data = poi_timeseries,
    aes(x = mean_C1, y = mean_C2),
    size = 0.8
  ) +
  geom_errorbar(
    data = poi_timeseries,
    aes(
      x = mean_C1, 
      ymin = mean_C2 - sd_C2, ymax = mean_C2 + sd_C2,
      color = z
    ),
    size = 0.4,
  ) +
  geom_errorbarh(
    data = poi_timeseries,
    aes(
      y = mean_C2, 
      xmin = mean_C1 - sd_C1, xmax = mean_C1 + sd_C1,
      color = z
    ),
    size = 0.4,
  ) +
  geom_point(
    data = poi_timeseries,
    aes(
      x = mean_C1, 
      y = mean_C2,
      color = z
    ),
    size = 2
  ) +
  age_colors_gradient +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_blank()
  ) +
  coord_fixed(xlim = c(-0.05, 0.08), ylim = c(-0.1, 0.065)) +
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  scale_x_continuous(breaks = seq(-0.08, 0.1, 0.04)) +
  guides(
    color = guide_legend(title = "Prediction time", nrow = 2, byrow = T)
  )

ggsave(
  "plots/figure_sup_11_timepillars.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 220, height = 300, units = "mm",
  limitsize = F
)
