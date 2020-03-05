library(ggplot2)

load("data/mobility_estimation/pri_mean.RData")

p <- pri_mean %>%
  dplyr::mutate(
    kernel_setting_id = dplyr::recode(
      kernel_setting_id, "ds100_dt200_g01" = "small kernel", "ds200_dt400_g01" = "big kernel"
    )
  ) %>%
  ggplot() +
  geom_line(
    aes(
      x = age_sample, y = mean_km_per_decade, 
      group = interaction(independent_table_id, kernel_setting_id), 
      color = kernel_setting_id
    ),
    alpha = 0.1
  ) +
  # geom_point(
  #   data = pri_mean %>% dplyr::summarise()
  #   aes(x = age_sample, y = 20, fill = mean_angle),
  #   shape = 21
  # ) +
  facet_wrap(~region_id, nrow = 3) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  xlab("time calBC [y]") +
  ylab("\"Speed\" [km/decade]") +
  guides(
    color = guide_legend(title = "kernels", override.aes = list(size = 10, alpha = 1))
  ) +
  scale_color_manual(
    values = c(
      "small kernel" = "orange",
      "big kernel" = "darkgreen"
    )
  ) +
  # scale_fill_gradient2(
  #   low = "blue",
  #   mid = "red",
  #   high = "blue",
  #   midpoint = 180
  # ) +
  NULL

ggsave(
  paste0("plots/mobility_estimation_regional_timeseries.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.5,
  dpi = 300,
  width = 550, height = 260, units = "mm",
  limitsize = F
)
