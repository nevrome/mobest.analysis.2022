load("data/gstats_spacetime_variogram.RData")
load("data/d_cut.R")

d_cut <- d_cut %>% dplyr::filter(
  time_dist_cut < 7000,
  geo_dist_cut < 4000
)

var <- var %>% tibble::as_tibble() %>%
  dplyr::mutate(
    timelag = (timelag - 0.5) * 100,
    spacelag = (spacelag - 50000) / 1000
  ) %>% dplyr::filter(
    timelag < 7000,
    spacelag < 4000
  )

ggplot() +
  geom_raster(
    data = d_cut,
    aes(geo_dist_cut, time_dist_cut, fill = mean_pca_dist)
  ) +
  viridis::scale_fill_viridis(direction = -1) +
  theme(legend.position = "bottom")

ggplot() +
  geom_raster(
    data = var,
    aes(spacelag, timelag, fill = gamma)
  ) +
  viridis::scale_fill_viridis(direction = -1) +
  theme(legend.position = "bottom")


