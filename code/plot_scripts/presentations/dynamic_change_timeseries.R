library(magrittr)
library(ggplot2)

interpol_grid_with_change <- interpol_grid %>%
  dplyr::group_by(
    dependent_var_id, x, y
  ) %>%
  dplyr::arrange(z, .by_group = TRUE) %>%
  dplyr::mutate(
    change = mean - dplyr::lag(mean)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(
    dependent_var_id
  ) %>%
  dplyr::mutate(
    sd_norm = sd/diff(range(mean))
  ) %>% 
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    id_cols = c("x", "y", "z"),
    names_from = dependent_var_id,
    values_from = c("change", "sd_norm")
  ) %>% 
  # dplyr::filter(
  #   dplyr::across(tidyselect::starts_with("change_"), ~!is.na(.x))
  # ) %>%
  dplyr::mutate(
    change_combined = sqrt(change_C1^2 + change_C2^2),
    mean_sd_norm = (sd_norm_C1 + sd_norm_C2)/2
  )

interpol_grid_with_change %>%
  dplyr::filter(
    z %% 200 == 0
  ) %>%
  ggplot() +
  geom_raster(aes(x, y, fill = change_combined)) +
  # geom_point(
  #   data = . %>% dplyr::filter(sd_to_high),
  #   mapping = aes(x, y),
  #   shape = 4, size = 0.5, color = "red"
  # ) +
  facet_wrap(~z) +
  scale_fill_viridis_c()

load("data/spatial/epsg102013.RData")
load("data/spatial/mobility_regions.RData")

iwr <- interpol_grid_with_change %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = epsg102013) %>%
  sf::st_intersection(mobility_regions) %>%
  sf::st_drop_geometry()

iwrs <- iwr %>%
  dplyr::group_by(
    region_id, z
  ) %>%
  dplyr::summarise(
    mean_change_combined = median(change_combined),
    mean_sd_norm = mean(mean_sd_norm)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    movavg = slider::slide_dbl(mean_change_combined, mean, .before = 4, .after = 4)
  )

iwrs$region_id = factor(iwrs$region_id, levels = c(
  "Britain and Ireland",
  "France", 
  "Iberia",
  "Italy",
  "Central Europe",
  "Eastern Europe",
  "Southeastern Europe",
  "Turkey",
  "Caucasus",
  "Near East"
))

iwrs %>%
  ggplot() +
  geom_line(
    aes(
      x = z, y = mean_change_combined, color = mean_sd_norm
    )
  ) +
  geom_line(
    aes(
      x = z, y = movavg
    ),
    color = "blue"
  ) +
  facet_grid(cols = dplyr::vars(region_id)) +
  scale_color_gradient(low = "green", high = "red")

