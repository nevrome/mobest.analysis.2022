library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")
load("data/plot_reference_data/age_colors_gradient.RData")

# normal mds plot
p <- ggplot() +
  geom_point(
    data = janno_final,
    aes(
      x = C1, y = C2, 
      color = Date_BC_AD_Median_Derived,
      shape = region_id
    ),
    size = 2,
    shape = 3
  ) +
  age_colors_gradient +
  coord_fixed(xlim = c(-0.05, 0.08), ylim = c(-0.095, 0.06)) +
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  scale_x_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  theme_bw() +
  theme(
    legend.position = "left",
    legend.direction = "vertical",
    legend.background = element_blank(),
    legend.title = element_text(size = 13),
    legend.spacing.y = unit(0.2, 'cm'),
    legend.key.height = unit(0.4, 'cm'),
    legend.text = element_text(size = 9),
  ) +
  guides(
    color = guide_colorbar(title = "Time", barwidth = 1.5, barheight = 20)
  )

ggsave(
  paste0("plots/presentation/simple_mds.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.7,
  dpi = 300,
  width = 300, height = 300, units = "mm",
  limitsize = F
)

