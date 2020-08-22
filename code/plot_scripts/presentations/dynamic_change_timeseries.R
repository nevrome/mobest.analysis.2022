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
    sd_to_high = sd > (0.20 * diff(range(mean)))
  ) %>% 
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    id_cols = c("x", "y", "z"),
    names_from = dependent_var_id,
    values_from = c("change", "sd_to_high")
  ) %>% 
  # dplyr::filter(
  #   dplyr::across(tidyselect::starts_with("change_"), ~!is.na(.x))
  # ) %>%
  dplyr::mutate(
    change_combined = sqrt(change_C1^2 + change_C2^2),
    sd_to_high = sd_to_high_C1 | sd_to_high_C2
  )

interpol_grid_with_change %>%
  dplyr::filter(
    z %% 200 == 0
  ) %>%
  ggplot() +
  geom_raster(aes(x, y, fill = change_combined)) +
  geom_point(
    data = . %>% dplyr::filter(sd_to_high),
    mapping = aes(x, y),
    shape = 4, size = 0.5, color = "red"
  ) +
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
    proportion_sd_to_high = sum(sd_to_high)/dplyr::n()
  ) %>%
  dplyr::ungroup()

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
      x = z, y = mean_change_combined, alpha = 1/proportion_sd_to_high
    )
  ) +
  facet_grid(cols = dplyr::vars(region_id))
