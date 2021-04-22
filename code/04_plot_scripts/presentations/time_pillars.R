library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")
load("data/plot_reference_data/region_id_shapes.RData")
load("data/plot_reference_data/age_colors_gradient.RData")
load("data/gpr/interpol_grid_examples.RData")

poi_timeseries <- interpol_grid_examples %>%
  tidyr::pivot_wider(
    id_cols = c("z", "kernel_setting_id", "pred_grid_id"),
    names_from = "dependent_var_id",
    values_from = c("mean", "sd")
  )

poi_timeseries_Barcelona <- poi_timeseries %>% dplyr::filter(pred_grid_id == "Barcelona")
poi_timeseries_Dnipro <- poi_timeseries %>% dplyr::filter(pred_grid_id == "Dnipro")
poi_timeseries_Jerusalem <- poi_timeseries %>% dplyr::filter(pred_grid_id == "Jerusalem")

# Barcelona
p_Barcelona <- ggplot() +
  geom_point(
    data = janno_final,
    aes(x = C1, y = C2),
    alpha = 0.1, size = 1, shape = 3
  ) +  
  geom_path(
    data = poi_timeseries_Barcelona,
    aes(x = mean_C1, y = mean_C2),
    size = 0.8
  ) +
  geom_errorbar(
    data = poi_timeseries_Barcelona,
    aes(
      x = mean_C1, 
      ymin = mean_C2 - sd_C2, ymax = mean_C2 + sd_C2,
      color = z
    ),
    size = 0.4,
  ) +
  geom_errorbarh(
    data = poi_timeseries_Barcelona,
    aes(
      y = mean_C2, 
      xmin = mean_C1 - sd_C1, xmax = mean_C1 + sd_C1,
      color = z
    ),
    size = 0.4,
  ) +
  geom_point(
    data = poi_timeseries_Barcelona,
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
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_fixed(xlim = c(-0.1, 0.05), ylim = c(-0.09, 0.065)) +
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  scale_x_continuous(breaks = seq(-0.08, 0.1, 0.04)) +
  ggtitle("Barcelona")

# Dnipro
p_Dnipro <- ggplot() +
  geom_point(
    data = janno_final,
    aes(x = C1, y = C2),
    alpha = 0.1, size = 1, shape = 3
  ) +  
  geom_path(
    data = poi_timeseries_Dnipro,
    aes(x = mean_C1, y = mean_C2),
    size = 0.8
  ) +
  geom_errorbar(
    data = poi_timeseries_Dnipro,
    aes(
      x = mean_C1, 
      ymin = mean_C2 - sd_C2, ymax = mean_C2 + sd_C2,
      color = z
    ),
    size = 0.4,
  ) +
  geom_errorbarh(
    data = poi_timeseries_Dnipro,
    aes(
      y = mean_C2, 
      xmin = mean_C1 - sd_C1, xmax = mean_C1 + sd_C1,
      color = z
    ),
    size = 0.4,
  ) +
  geom_point(
    data = poi_timeseries_Dnipro,
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
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_fixed(xlim = c(-0.1, 0.05), ylim = c(-0.09, 0.065)) +
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  scale_x_continuous(breaks = seq(-0.08, 0.1, 0.04)) +
  ggtitle("Dnipro")

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
  age_colors_gradient +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_fixed(xlim = c(-0.1, 0.05), ylim = c(-0.09, 0.065)) +
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  scale_x_continuous(breaks = seq(-0.08, 0.1, 0.04)) +
  ggtitle("Jerusalem")

# merge plots

p <- cowplot::plot_grid(p_Dnipro, p_Barcelona, p_Jerusalem, nrow = 1, rel_widths = c(1, 1, 1))

ggsave(
  paste0("plots/time_pillars.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.3,
  dpi = 300,
  width = 800, height = 300, units = "mm",
  limitsize = F
)

