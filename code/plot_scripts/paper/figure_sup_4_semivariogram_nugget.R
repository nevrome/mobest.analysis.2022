library(magrittr)
library(ggplot2)

load("data/parameter_exploration/variogram/all_distances.RData")
load("data/poseidon_data/janno_final.RData")

d_all_long <- d_all %>% tidyr::pivot_longer(
  cols = c(C1_dist_resid, C2_dist_resid),
  names_to = "dist_type", values_to = "dist_val"
)

lower_left <- d_all_long %>%
  dplyr::filter(time_dist < 50 & geo_dist < 50) %>%
  dplyr::filter(Var1 != Var2) %>%
  dplyr::mutate(
    dist_type = replace(dist_type, dist_type == "C1_dist_resid", "C1"),
    dist_type = replace(dist_type, dist_type == "C2_dist_resid", "C2"),
    dist_type = factor(dist_type, levels = c("C1", "C2")),
    # rescaling of the dist val to a relative proportion
    dist_val_adjusted = ifelse(
      dist_type == "C1",
      0.5*(dist_val^2/var(janno_final$C1)),
      0.5*(dist_val^2/var(janno_final$C2))
    )
  )

lower_left_mean <- lower_left %>%
  dplyr::group_by(
    dist_type
  ) %>%
  dplyr::summarise(
    mean = mean(dist_val_adjusted, na.rm = T)
  )

p <- ggplot() +
  geom_jitter(
    data = lower_left,
    mapping = aes(x = dist_type, y = dist_val_adjusted, color = dist_type),
    alpha = 0.5,
    size = 0.5,
    width = 0.4
  ) + 
  geom_boxplot(
    data = lower_left,
    mapping = aes(x = dist_type, y = dist_val_adjusted),
    alpha = 0.5,
    width = 0.5
  ) +
  geom_text(
    data = lower_left_mean,
    mapping = aes(x = dist_type, y = mean, label = paste0("mean: ~", round(mean, 3))),
    nudge_x = -0.5
  ) +
  coord_flip() +
  theme_bw() +
  guides(
    color = F
  ) +
  xlab("ancestry component distance type") +
  ylab("log10 pairwise normalized residual distance") +
  scale_y_log10(labels = scales::comma) +
  scale_x_discrete(limits = rev(levels(lower_left$dist_type)))

ggsave(
  "plots/figure_sup_4_semivariogram_nugget.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 120, units = "mm",
  limitsize = F
)
