library(ggplot2)
library(magrittr)

load("data/parameter_exploration/variogram/all_distances.RData")

d_all_long <- d_all %>% tidyr::pivot_longer(
  cols = c(C1_dist_resid, C2_dist_resid),
  names_to = "dist_type", values_to = "dist_val"
) %>%
  dplyr::mutate(
   dist_val = dist_val
  )

bottom_space <- d_all_long %>%
  dplyr::filter(time_dist < 50) %>%
  dplyr::filter(geo_dist != 0 & time_dist != 0) %>%
  dplyr::mutate(
    dist_type = replace(dist_type, dist_type == "C1_dist_resid", "C1"),
    dist_type = replace(dist_type, dist_type == "C2_dist_resid", "C2"),
    dist_type = factor(dist_type, levels = c("C1", "C2")),
    geo_dist_cut = (cut(
      geo_dist, 
      breaks = seq(0, max(geo_dist), 100), 
      include.lowest	= T, 
      labels = F
    ) * 100) - 50
  )

bottom_space_grouped <- bottom_space %>%
  dplyr::group_by(geo_dist_cut, dist_type) %>%
  dplyr::summarise(
    mean_dist_val = 0.5*mean(dist_val, na.rm = T)
  )

left_time <- d_all_long %>%
  dplyr::filter(geo_dist < 50) %>%
  dplyr::filter(geo_dist != 0 & time_dist != 0) %>%
  dplyr::mutate(
    dist_type = replace(dist_type, dist_type == "C1_dist_resid", "C1"),
    dist_type = replace(dist_type, dist_type == "C2_dist_resid", "C2"),
    dist_type = factor(dist_type, levels = c("C1", "C2")),
    time_dist_cut = (cut(
      time_dist, 
      breaks = seq(0, max(time_dist), 100), 
      include.lowest = T, 
      labels = F
    ) * 100) - 50
  )
 
left_time_grouped <- left_time %>%
  dplyr::group_by(time_dist_cut, dist_type) %>%
  dplyr::summarise(
    mean_dist_val = 0.5*mean(dist_val, na.rm = T)
  )

p_space <- ggplot() + 
  geom_point(
    data = bottom_space,
    mapping = aes(
      x = geo_dist,
      y = dist_val,
      col = dist_type
    ),
    size = 0.1,
    alpha = 0.3
  ) +
  theme_bw() +
  geom_line(
    data = bottom_space_grouped,
    mapping = aes(
      x = geo_dist_cut,
      y = mean_dist_val
    )
  ) +
  facet_wrap(~dist_type) +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    color = FALSE
  ) + 
  xlab("spatial distance in kilometres") +
  ylab("ancestry component distance      ")
  

p_time <- ggplot(left_time) + 
  geom_point(
    data = left_time,
    mapping = aes(
      x = time_dist,
      y = dist_val,
      col = dist_type
    ),
    size = 0.1,
    alpha = 0.3
  ) +
  theme_bw() +
  geom_line(
    data = left_time_grouped,
    mapping = aes(
      x = time_dist_cut,
      y = mean_dist_val
    )
  ) +
  facet_wrap(~dist_type) +
  guides(
    color = FALSE
  ) +
  xlab("temporal distance in years") +
  ylab("ancestry component distance      ")


p <- cowplot::plot_grid(p_space, p_time, nrow = 2, labels = c("A", "B"))

ggsave(
  "plots/figure_sup_2_semivariogram_space_time.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.4,
  dpi = 300,
  width = 500, height = 300, units = "mm",
  limitsize = F
)
