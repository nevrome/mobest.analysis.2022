library(magrittr)
library(ggplot2)

load("data/parameter_exploration/variogram/lower_left_variogram.RData")
load("data/parameter_exploration/variogram/estimated_nuggets.RData")

lower_left_variogram %<>% dplyr::filter(dist_type %in% c("C1", "C2"))
estimated_nuggets %<>% dplyr::filter(dist_type %in% c("C1", "C2"))

p <- ggplot() +
  geom_jitter(
    data = lower_left_variogram,
    mapping = aes(x = dist_type, y = dist_val_adjusted, color = dist_type),
    alpha = 0.5,
    size = 0.5,
    width = 0.4
  ) + 
  geom_point(
    data = estimated_nuggets,
    mapping = aes(x = dist_type, y = mean),
    size = 2
  ) +
  geom_point(
    data = estimated_nuggets,
    mapping = aes(x = dist_type, y = mean),
    size = 5, shape = "|"
  ) +
  geom_text(
    data = estimated_nuggets,
    mapping = aes(x = dist_type, y = mean, label = paste0("mean: ~", round(mean, 3))),
    nudge_x = -0.5
  ) +
  coord_flip() +
  theme_bw() +
  guides(
    color = "none"
  ) +
  xlab("ancestry component distance type") +
  ylab("log10 pairwise half mean squared normalized residual distance") +
  scale_y_log10(labels = scales::comma) +
  scale_x_discrete(limits = rev(unique(lower_left_variogram$dist_type)))

ggsave(
  "plots/figure_sup_4_semivariogram_nugget.pdf",
  plot = p,
  device = "pdf",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 120, units = "mm",
  limitsize = F
)

