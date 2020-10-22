library(magrittr)
library(ggplot2)

load("data/mobility_estimation/mobility_proxy.RData")

mobility_proxy$region_id <- factor(
  mobility_proxy$region_id, levels = c(
    "Britain and Ireland",
    "Central Europe",
    "Eastern Europe",
    "Caucasus",
    "France",
    "Italy",
    "Southeastern Europe",
    "Turkey",
    "Iberia",
    "Levant"
  )
)

p <- mobility_proxy %>%
  # dplyr::mutate(
  #   kernel_setting_id = dplyr::recode(
  #     kernel_setting_id, "ds100_dt200_g01" = "small kernel", "ds200_dt400_g01" = "big kernel"
  #   )
  # ) %>%
  ggplot() +
  geom_line(
    aes(
      x = z, y = mean_km_per_decade, 
      group = interaction(independent_table_id, kernel_setting_id), 
      color = angle_deg
    ),
    alpha = 0.5
  ) +
  facet_wrap(~region_id, nrow = 3) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  xlab("time calBC [y]") +
  ylab("\"Speed\" [km/decade]") +
  # guides(
  #   color = guide_legend(title = "kernels", override.aes = list(size = 10, alpha = 1))
  # ) +
  # scale_color_manual(
  #   values = c(
  #     "small kernel" = "orange",
  #     "big kernel" = "darkgreen"
  #   )
  # ) +
  # scale_fill_gradient2(
  #   low = "blue",
  #   mid = "red",
  #   high = "blue",
  #   midpoint = 180
  # ) +
  scale_color_gradientn(colours = c("blue", "red", "yellow", "green", "blue")) +
  NULL

ggsave(
  paste0("plots/mobility_estimation_regional_timeseries.png"),
  plot = p,
  device = "png",
  scale = 0.5,
  dpi = 300,
  width = 550, height = 260, units = "mm",
  limitsize = F
)
