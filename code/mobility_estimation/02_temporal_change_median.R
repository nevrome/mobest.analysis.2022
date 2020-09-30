library(magrittr)

#### data ####
load("data/gpr/interpol_grid_median.RData")

#### calculate change ####
interpol_grid_with_change <- interpol_grid %>%
  dplyr::group_by(
    kernel_setting_id, dependent_var_id, x, y
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
    id_cols = c("kernel_setting_id", "region_id", "x", "y", "z"),
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

save(interpol_grid_with_change, file = "data/gpr/interpol_grid_median_with_change.RData")

iwrs <- interpol_grid_with_change %>%
  dplyr::group_by(
    kernel_setting_id, region_id, z
  ) %>%
  dplyr::summarise(
    mean_change_combined = median(change_combined),
    mean_sd_norm = mean(mean_sd_norm)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    movavg = slider::slide_dbl(mean_change_combined, mean, .before = 4, .after = 4)
  )

temporal_change <- iwrs

save(temporal_change, file = "data/gpr/temporal_change_median.RData")

