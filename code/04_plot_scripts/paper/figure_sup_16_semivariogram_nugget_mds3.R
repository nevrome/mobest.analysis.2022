library(magrittr)
library(ggplot2)

load("data/parameter_exploration/variogram/lower_left_variogram.RData")
load("data/parameter_exploration/variogram/estimated_nuggets.RData")

lower_left_variogram %<>% dplyr::filter(dist_type %in% c("C3"))
estimated_nuggets %<>% dplyr::filter(dist_type %in% c("C3"))

p <- ggplot() +
  geom_violin(
    data = lower_left_variogram,
    mapping = aes(x = dist_type, y = dist_val_adjusted),
    size = 0.5,
    width = 0.8,
    fill = "#7CAE00"
  ) +
  geom_boxplot(
    data = lower_left_variogram,
    mapping = aes(x = dist_type, y = dist_val_adjusted),
    width = 0.1
  ) +
  geom_point(
    data = estimated_nuggets,
    mapping = aes(x = dist_type, y = mean),
    size = 4, shape = 18
  ) +
  geom_point(
    data = estimated_nuggets,
    mapping = aes(x = dist_type, y = mean),
    size = 6, shape = "|"
  ) +
  geom_text(
    data = estimated_nuggets,
    mapping = aes(x = dist_type, y = mean, label = paste0("mean: ~", round(mean, 3))),
    nudge_x = -0.5
  ) +
  coord_flip() +
  theme_bw() +
  guides(fill = "none") +
  xlab("") +
  ylab("log10 pairwise half mean squared normalized residual distance") +
  scale_y_log10(labels = scales::comma, oob = scales::squish) +
  scale_x_discrete(limits = rev(unique(lower_left_variogram$dist_type)))

ggsave(
  "plots/figure_sup_16_semivariogram_nugget_mds3.pdf",
  plot = p,
  device = "pdf",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 70, units = "mm",
  limitsize = F
)

