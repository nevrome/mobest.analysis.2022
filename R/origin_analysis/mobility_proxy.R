load("data/pri_ready_spatial.RData")
load("data/spatial/mobility_regions.RData")

pri <- pri_ready_spatial %>%
  sf::st_intersection(mobility_regions)

pri_mean <- pri %>%
  dplyr::group_by(
    age_sample, region_id
  ) %>%
  dplyr::summarise(
    mean_km_per_decade = mean(spatial_distance)/1000/10
  )

library(ggplot2)
pri_mean %>%
  ggplot() +
  geom_line(aes(x = age_sample, y = mean_km_per_decade, group = region_id, color = region_id)) +
  facet_wrap(~region_id)

