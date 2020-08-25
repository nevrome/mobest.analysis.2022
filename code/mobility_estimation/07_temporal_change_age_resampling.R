library(magrittr)

#### data ####
load("data/poseidon_data/janno_final.RData")
interpol_grid_age_resampling <- lapply(
  list.files("data/gpr/age_resampling", full.names = T),
  function(x) {
    load(x)
    interpol_grid
  }
) %>% dplyr::bind_rows() %>%
  # kernel selection
  dplyr::mutate(
    kernel_setting_id = dplyr::recode(
      kernel_setting_id, 
      "ds400_dt200_g001" = "400km / 200y", 
      "ds600_dt300_g001" = "600km / 300y",
      "ds800_dt400_g001" = "800km / 400y"
    )
  )

#### calculate change ####
interpol_grid_with_change <- interpol_grid_age_resampling %>%
  dplyr::group_by(
    independent_table_id, kernel_setting_id, dependent_var_id, x, y
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
    sd_norm = ifelse(
      dependent_var_id == "C1",
      sd/diff(c(min(janno_final$C1), max(janno_final$C1))),
      sd/diff(c(min(janno_final$C2), max(janno_final$C2)))
    )
  ) %>% 
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    id_cols = c("independent_table_id", "kernel_setting_id", "x", "y", "z"),
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

load("data/spatial/epsg102013.RData")
load("data/spatial/mobility_regions.RData")

iwr <- interpol_grid_with_change %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = epsg102013) %>%
  sf::st_intersection(mobility_regions) %>%
  sf::st_drop_geometry()

iwr$region_id = factor(iwr$region_id, levels = c(
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

iwrs_age_resampling_run <- iwr %>%
  dplyr::group_by(
    independent_table_id, kernel_setting_id, region_id, z
  ) %>%
  dplyr::summarise(
    mean_change_combined = mean(change_combined),
    gpr_mean_sd_norm = mean(mean_sd_norm),
  ) %>%
  dplyr::ungroup()

iwrs_age_total <- iwrs_age_resampling_run %>%
  dplyr::group_by(
    kernel_setting_id, region_id, z
  ) %>%
  dplyr::summarise(
    mean_mean_change_combined = mean(mean_change_combined),
    sd_mean_change_combined = sd(mean_change_combined) 
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    movavg_mean = slider::slide_dbl(mean_mean_change_combined, mean, .before = 4, .after = 4),
    movavg_sd = slider::slide_dbl(sd_mean_change_combined, mean, .before = 4, .after = 4)
  )

save(iwrs_age_resampling_run, iwrs_age_total, file = "data/gpr/temporal_change_age_resampling.RData")

