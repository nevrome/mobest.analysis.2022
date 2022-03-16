library(magrittr)
library(ggplot2)

load("data/genotype_data/janno_final.RData")
load("data/gpr/interpol_grid_examples.RData")
load("data/plot_reference_data/age_colors_gradient.RData")

poi_timeseries <- interpol_grid_examples %>%
  tidyr::pivot_wider(
    id_cols = c("z", "kernel_setting_id", "pred_grid_id"),
    names_from = "dependent_var_id",
    values_from = c("mean", "sd")
  )

p <- ggplot() +
  facet_wrap(~pred_grid_id, nrow = 3) +
  geom_point(
    data = janno_final,
    aes(x = C1, y = C2),
    alpha = 0.1, size = 1, shape = 3
  ) +  
  geom_path(
    data = poi_timeseries,
    aes(x = mean_C1, y = mean_C2),
    size = 0.8,
    color = "lightgrey"
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
    legend.position = "right"
  ) +
  coord_fixed() +
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.04), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(-0.08, 0.1, 0.04)) +
  guides(
    color = guide_legend(title = "Prediction time", ncol = 1)
  )
  

ggsave(
  "plots/figure_sup_11_timepillars.pdf",
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 255, height = 300, units = "mm",
  limitsize = F
)
