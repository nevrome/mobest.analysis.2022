library(magrittr)
library(ggplot2)

load("data/parameter_exploration/targeted/lower_left_variogram.RData")
load("data/parameter_exploration/targeted/estimated_nuggets.RData")

p <- ggplot() +
  geom_violin(
    data = lower_left_variogram,
    mapping = aes(x = dist_type, y = dist_val_adjusted, fill = dist_type),
    size = 0.5,
    width = 0.8
  ) +
  geom_boxplot(
    data = lower_left_variogram,
    mapping = aes(x = dist_type, y = dist_val_adjusted),
    width = 0.1, outlier.size = 1
  ) +
  geom_point(
    data = estimated_nuggets,
    mapping = aes(x = dist_type, y = nugget),
    size = 4, shape = 18
  ) +
  geom_point(
    data = estimated_nuggets,
    mapping = aes(x = dist_type, y = nugget),
    size = 6, shape = "|"
  ) +
  geom_text(
    data = estimated_nuggets,
    mapping = aes(x = dist_type, y = nugget, label = paste0("mean: ~", round(nugget, 3))),
    nudge_x = -0.5
  ) +
  coord_flip() +
  theme_bw() +
  guides(fill = "none") +
  xlab("ancestry component distance type") +
  ylab("pairwise half mean squared normalized residual distance") +
  scale_y_log10(labels = scales::comma) +
  scale_x_discrete(limits = rev(unique(lower_left_variogram$dist_type)))

ggsave(
  "plots/figure_sup_4_semivariogram_nugget.pdf",
  plot = p,
  device = "pdf",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 150, units = "mm",
  limitsize = F
)

