library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")
load("data/plot_reference_data/region_id_colors.RData")
load("data/plot_reference_data/age_group_id_shapes.RData")

p_mds <- ggplot() +
  geom_point(
    data = janno_final,
    aes(x = C1, y = C2, color = region_id, shape = age_group_id),
    alpha = 0.7,
    size = 2
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.background = element_blank(),
    legend.title = element_text(size = 13),
    legend.spacing.x = unit(0.2, 'cm'),
    legend.key.height = unit(0.4, 'cm'),
    legend.text = element_text(size = 9),
  ) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  scale_shape_manual(
    values = age_group_id_shapes
  ) +
  scale_color_manual(
    values = region_id_colors
  ) +
  guides(
    color = guide_legend(title = "Region", nrow = 3, ncol = 4),
    shape = guide_legend(title = "Time", nrow = 3, ncol = 4)
  ) +
  coord_fixed() +
  scale_y_continuous(breaks = seq(-0.08, 0.08, 0.02)) +
  scale_x_continuous(breaks = seq(-0.06, 0.08, 0.02))

ggsave(
  paste0("plots/MDS_plot.jpeg"),
  plot = p_mds,
  device = "jpeg",
  scale = 0.3,
  dpi = 300,
  width = 630, height = 600, units = "mm",
  limitsize = F
)
