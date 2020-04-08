load("data/crossvalidation/interpol_comparison_group.RData")

library(magrittr)
library(ggplot2)

interpol_comparison_group %>%
  ggplot() +
  geom_raster(
    aes(x = ds, y = dt, fill = sd_difference)
  ) +
  scale_fill_viridis_c() +
  facet_grid(rows = vars(PC), cols = vars(g))


icg <- interpol_comparison_group %>%
  dplyr::group_by(
    ds, dt, g
  ) %>%
  dplyr::summarise(
    mean_mean_difference = mean(mean_difference),
    mean_median_difference = mean(median_difference),
    mean_sd_difference = mean(sd_difference),
    mean_diff_5_95_difference = mean(diff_5_95_difference)
  ) %>%
  dplyr::ungroup()


minicg <- icg %>% dplyr::filter(
  mean_median_difference == min(mean_median_difference)
)

ggplot() +
  geom_raster(
    data = icg,
    aes(x = ds, y = dt, fill = mean_median_difference)
  ) +
  scale_fill_viridis_c() +
  facet_wrap(~g) +
  geom_point(
    data = minicg,
    aes(x = ds, y = dt),
    shape = 4,
    color = "red",
    size = 3
  )




  
  