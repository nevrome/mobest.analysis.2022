library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")
load("data/plot_reference_data/region_id_colors.RData")
load("data/plot_reference_data/age_group_id_shapes.RData")

p <- ggplot() +
  geom_point(
    data = janno_final,
    aes(x = C1, y = C2, color = region_id, shape = age_group_id),
    alpha = 0.7,
    size = 2
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
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
  coord_fixed() +
  scale_y_reverse(breaks = seq(-0.08, 0.08, 0.02)) +
  scale_x_continuous(breaks = seq(-0.06, 0.08, 0.02))

ggsave(
  paste0("plots/figure_6_mds.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.7,
  dpi = 300,
  width = 300, height = 280, units = "mm",
  limitsize = F
)
