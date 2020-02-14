library(ggplot2)
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")

ggplot() +
  geom_sf(
    data = extended_area,
    fill = "white"
  ) +
  # geom_segment(
  #   data = pri_ready,
  #   mapping = aes(x = x_real, y = y_real, xend = x_real_origin, yend = y_real_origin),
  #   arrow = arrow(length = unit(.05, 'inches')),
  #   color = "red",
  #   alpha = 0.2
  # ) +
  # geom_raster(
  #   data = pri_ready, 
  #   aes(x = x_real, y = y_real, fill = pred_1)
  # ) +
geom_spoke(
  data = pri_ready %>% dplyr::filter(!((x_real == x_real_origin) & (y_real == y_real_origin))),
  mapping = aes(x = x_real, y_real, angle = angle, size = spatial_distance, alpha = spatial_distance),
  radius = 200000,
  arrow = arrow(length = unit(.05, 'inches')),
  color = "blue"
) +
  # geom_text(
  #   data = pri_ready,
  #   mapping = aes(x_real, y_real, label = 1:nrow(pri_ready)),
  #   size = 1.7
  # ) +
  facet_wrap(~age_sample) +
  scale_size_continuous(
    guide = FALSE,
    range = c(0.01, 1.5)
  ) +
  scale_alpha_continuous(
    guide = FALSE,
    range = c(0.5, 1)
  ) +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
