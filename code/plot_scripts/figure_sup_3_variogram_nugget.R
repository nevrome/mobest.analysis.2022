library(magrittr)
library(ggplot2)

load("data/parameter_exploration/variogram/all_distances.RData")

d_all_long <- d_all %>% tidyr::pivot_longer(
  cols = c(PC1_dist_resid, PC2_dist_resid, C1_dist_resid, C2_dist_resid),
  names_to = "dist_type", values_to = "dist_val"
)

lower_left <- d_all_long %>%
  dplyr::filter(time_dist < 50 & geo_dist < 50) %>%
  dplyr::filter(geo_dist != 0 & time_dist != 0) %>%
  dplyr::mutate(
    dist_type = replace(dist_type, dist_type == "PC1_dist_resid", "PC1"),
    dist_type = replace(dist_type, dist_type == "PC2_dist_resid", "PC2"),
    dist_type = replace(dist_type, dist_type == "C1_dist_resid", "C1"),
    dist_type = replace(dist_type, dist_type == "C2_dist_resid", "C2"),
    dist_type = factor(dist_type, levels = c("PC1", "PC2", "C1", "C2"))
  )

lower_left_median <- lower_left %>%
  dplyr::group_by(
    dist_type
  ) %>%
  dplyr::summarise(
    median = median(dist_val, na.rm = T)
  )

p <- ggplot() +
  geom_jitter(
    data = lower_left,
    mapping = aes(x = dist_type, y = dist_val, color = dist_type),
    alpha = 0.5,
    size = 0.5,
    width = 0.4
  ) + 
  geom_boxplot(
    data = lower_left,
    mapping = aes(x = dist_type, y = dist_val),
    alpha = 0.5,
    width = 0.5
  ) +
  geom_text(
    data = lower_left_median,
    mapping = aes(x = dist_type, y = median, label = paste0("median: ~", round(median, 3))),
    nudge_x = -0.5
  ) +
  coord_flip() +
  theme_bw() +
  guides(
    color = F
  ) +
  xlab("ancestry component distance type") +
  ylab("log10 pairwise residual distance") +
  scale_y_log10(labels = scales::comma) +
  scale_x_discrete(limits = rev(levels(lower_left$dist_type)))

ggsave(
  "plots/figure_sup_3_variogram_nugget.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 200, units = "mm",
  limitsize = F
)
