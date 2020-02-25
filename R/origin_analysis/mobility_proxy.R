load("data/pri_ready_spatial.RData")
load("data/spatial/mobility_regions.RData")

pri <- pri_ready_spatial %>%
  sf::st_intersection(mobility_regions)

pri_mean <- pri %>%
  dplyr::group_by(
    age_sample, region_id
  ) %>%
  dplyr::summarise(
    mean_spatial_distance = mean(spatial_distance)
  )

library(ggplot2)
pri_mean %>%
  ggplot() +
  geom_line(aes(x = age_sample, y = mean_spatial_distance, group = region_id, color = region_id)) +
  facet_wrap(~region_id)

