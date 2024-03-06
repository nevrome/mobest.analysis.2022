library(magrittr)
library(ggplot2)

load("data/genotype_data/janno_final.RData")
load("data/plot_reference_data/age_colors_gradient.RData")

p <- ggplot() +
  geom_point(
    data = janno_final,
    aes(
      x = C1_mds_u, y = C2_mds_u, 
      color = Date_BC_AD_Median_Derived
    ),
    size = 2,
    shape = 3
  ) +
  age_colors_gradient +
  coord_fixed() +
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
  paste0("plots/presentation/simple_mds.png"),
  plot = p,
  device = "png",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 300, units = "mm",
  limitsize = F
)

