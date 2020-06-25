library(ggplot2)
library(magrittr)

load("data/parameter_exploration/variogram/all_distances.RData")

d_all_long <- d_all %>% tidyr::pivot_longer(
  cols = c(PC1_dist_resid, PC2_dist_resid, C1_dist_resid, C2_dist_resid),
  names_to = "dist_type", values_to = "dist_val"
)

bottom_space <- d_all_long %>%
  dplyr::filter(time_dist < 50) %>%
  dplyr::filter(geo_dist != 0 & time_dist != 0) %>%
  dplyr::mutate(
    dist_type = replace(dist_type, dist_type == "PC1_dist_resid", "PC1"),
    dist_type = replace(dist_type, dist_type == "PC2_dist_resid", "PC2"),
    dist_type = replace(dist_type, dist_type == "C1_dist_resid", "C1"),
    dist_type = replace(dist_type, dist_type == "C2_dist_resid", "C2"),
    dist_type = factor(dist_type, levels = c("PC1", "PC2", "C1", "C2") %>% rev)
  )

left_time <- d_all_long %>%
  dplyr::filter(geo_dist < 50) %>%
  dplyr::filter(geo_dist != 0 & time_dist != 0) %>%
  dplyr::mutate(
    dist_type = replace(dist_type, dist_type == "PC1_dist_resid", "PC1"),
    dist_type = replace(dist_type, dist_type == "PC2_dist_resid", "PC2"),
    dist_type = replace(dist_type, dist_type == "C1_dist_resid", "C1"),
    dist_type = replace(dist_type, dist_type == "C2_dist_resid", "C2"),
    dist_type = factor(dist_type, levels = c("PC1", "PC2", "C1", "C2") %>% rev)
  )
 
ggplot(bottom_space) + 
  geom_point(
    aes(
      x = geo_dist,
      y = dist_val,
      col = dist_type
    ),
    size = 0.1
  ) +
  theme_bw() +
  geom_smooth(
    aes(
      x = geo_dist,
      y = dist_val
    )
  ) +
  facet_wrap(~dist_type)

ggplot(left_time) + 
  geom_point(
    aes(
      x = time_dist,
      y = dist_val,
      col = dist_type
    ),
    size = 0.1
  ) +
  theme_bw() +
  geom_smooth(
    aes(
      x = time_dist,
      y = dist_val
    )
  ) +
  facet_wrap(~dist_type)


  # guides(
  #   color = guide_legend(title = "temporal distance: 500y bins")
  # ) +
  # xlab("spatial distance: 100km bins") +
  # ylab("mean squared euclidean distance in PC1 & PC2 PCA space")

ggsave(
  "plots/figure_sup_10_variogram_space_time.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 200, units = "mm",
  limitsize = F
)
