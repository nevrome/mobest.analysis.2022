library(magrittr)
library(ggplot2)

load("data/genotype_data/janno_final.RData")
load("data/origin_search/interpol_grid_specific_places.RData")
load("data/plot_reference_data/age_colors_gradient.RData")

poi_timeseries <- interpol_grid_examples %>%
  dplyr::mutate(
    pred_grid_id = factor(pred_grid_id, levels = c("London", "Riga", "Rome", "Jerusalem"))
  ) %>%
  tidyr::pivot_wider(
    id_cols = c("z", "kernel_setting_id", "pred_grid_id"),
    names_from = "dependent_var_id",
    values_from = c("mean", "sd")
  )

p <- ggplot() +
  facet_wrap(~pred_grid_id, nrow = 2) +
  geom_point(
    data = janno_final,
    aes(x = C1_mds_u, y = C2_mds_u),
    alpha = 0.1, size = 1, shape = 3
  ) +  
  geom_path(
    data = poi_timeseries,
    aes(x = mean_C1_mds_u, y = mean_C2_mds_u),
    size = 0.8,
    color = "lightgrey"
  ) +
  geom_errorbar(
    data = poi_timeseries,
    aes(
      x = mean_C1_mds_u, 
      ymin = mean_C2_mds_u - sd_C2_mds_u, ymax = mean_C2_mds_u + sd_C2_mds_u,
      color = z
    ),
    size = 0.4,
  ) +
  geom_errorbarh(
    data = poi_timeseries,
    aes(
      y = mean_C2_mds_u, 
      xmin = mean_C1_mds_u - sd_C1_mds_u, xmax = mean_C1_mds_u + sd_C1_mds_u,
      color = z
    ),
    size = 0.4,
  ) +
  geom_point(
    data = poi_timeseries,
    aes(
      x = mean_C1_mds_u, 
      y = mean_C2_mds_u,
      color = z
    ),
    size = 2
  ) +
  age_colors_gradient +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  coord_fixed() +
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.04), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(-0.08, 0.1, 0.04)) +
  guides(
    color = guide_legend(title = "Prediction time in years BC/AD  ", nrow = 2, byrow = T)
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
