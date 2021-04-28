library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")
load("data/plot_reference_data/region_id_shapes.RData")
load("data/plot_reference_data/region_id_fill.RData")
load("data/plot_reference_data/age_colors_gradient.RData")

# mean per region and time
region_age_group_mean <- janno_final %>%
  dplyr::filter(!is.na(region_id)) %>%
  dplyr::group_by(region_id, age_group_id) %>%
  dplyr::summarise(mean_C1 = mean(C1), mean_C2 = mean(C2), z = mean(Date_BC_AD_Median_Derived)) %>%
  dplyr::ungroup()

# normal mds plot
p <- ggplot() +
  geom_point(
    data = janno_final,
    aes(
      x = C1, y = C2, 
      color = Date_BC_AD_Median_Derived,
      shape = region_id
    ),
    size = 2
  ) +
  ggpointgrid::geom_pointgrid(
    data = region_age_group_mean,
    aes(x = mean_C1, y = mean_C2),
    size = 5,
    fill = "black",
    color = "black",
    shape = 21,
    grid_x = 23,
    grid_y = 23
  ) +
  ggpointgrid::geom_pointgrid(
    data = region_age_group_mean,
    aes(x = mean_C1, y = mean_C2, color = z, shape = region_id),
    size = 2,
    grid_x = 23,
    grid_y = 23,
    stroke = 1
  ) +
  scale_shape_manual(
    values = region_id_shapes,
    na.value = 3
  ) +
  age_colors_gradient +
  coord_fixed(xlim = c(-0.05, 0.08), ylim = c(-0.095, 0.06)) +
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  scale_x_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.background = element_blank(),
    legend.title = element_text(size = 13),
    legend.spacing.y = unit(0.2, 'cm'),
    legend.key.height = unit(0.4, 'cm'),
    legend.text = element_text(size = 10),
  ) +
  guides(
    color = guide_colorbar(title = "Time", barwidth = 20, barheight = 1.5),
    shape = guide_legend(title = "Region", nrow = 3, ncol = 4, byrow = F)
  )

ggsave(
  paste0("plots/figure_2_mds.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.7,
  dpi = 300,
  width = 220, height = 300, units = "mm",
  limitsize = F
)

