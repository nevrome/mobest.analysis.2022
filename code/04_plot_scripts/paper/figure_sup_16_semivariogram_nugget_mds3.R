library(magrittr)
library(ggplot2)

load("data/parameter_exploration/variogram/all_distances.RData")
load("data/poseidon_data/janno_final.RData")

d_all_long <- d_all %>% tidyr::pivot_longer(
  cols = c(C1_dist_resid, C2_dist_resid, C3_dist_resid),
  names_to = "dist_type", values_to = "dist_val"
) %>% dplyr::filter(
  dist_type == "C3_dist_resid"
)

lower_left <- d_all_long %>%
  dplyr::filter(time_dist < 50 & geo_dist < 50) %>%
  dplyr::filter(Var1 != Var2) %>%
  dplyr::mutate(
    dist_type = replace(dist_type, dist_type == "C3_dist_resid", "C3"),
    dist_val_adjusted = 0.5*(dist_val^2/var(janno_final$C3))
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
    mapping = aes(x = "C3", y = dist_val_adjusted),
    alpha = 0.5,
    size = 0.5,
    width = 0.4,
    color = "#7CAE00"
  ) + 
  geom_point(
    data = lower_left_mean,
    mapping = aes(x = dist_type, y = mean),
    size = 2
  ) +
  geom_point(
    data = lower_left_mean,
    mapping = aes(x = dist_type, y = mean),
    size = 5, shape = "|"
  ) +
  geom_text(
    data = lower_left_mean,
    mapping = aes(x = dist_type, y = mean, label = paste0("mean: ~", round(mean, 3))),
    nudge_x = -0.5
  ) +
  coord_flip() +
  theme_bw() +
  guides(
    color = "none"
  ) +
  xlab("") +
  ylab("log10 pairwise half mean squared normalized residual distance") +
  scale_y_log10(labels = scales::comma) +
  scale_x_discrete(limits = rev(levels(lower_left$dist_type)))

ggsave(
  "plots/figure_sup_16_semivariogram_nugget_mds3.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 70, units = "mm",
  limitsize = F
)
