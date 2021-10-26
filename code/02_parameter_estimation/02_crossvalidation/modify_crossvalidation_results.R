library(magrittr)

# read and rbind all crossvalidation run output files
interpol_comparison <- lapply(
  list.files(
    "data/parameter_exploration/crossvalidation", 
    pattern = "interpol_comparison_[0-9]", 
    full.names = T
  ), function(x) {
    load(x)
    interpol_comparison
  }
) %>% dplyr::bind_rows()

# interpol_comparison <- dplyr::slice_sample(interpol_comparison, n = 100000)

# calculate squared Euclidean distances
interpol_comparison_with_CVdist <- interpol_comparison %>%
  tidyr::pivot_wider(
    id_cols = c("id", "mixing_iteration", "ds", "dt", "g"),
    names_from = "dependent_var",
    values_from = "difference"
  ) %>%
  dplyr::mutate(
    CVdist2 = sqrt(C1_dist^2 + C2_dist^2),
    CVdist3 = sqrt(C1_dist^2 + C2_dist^2 + C3_dist^2)
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("C"),
    names_to = "dependent_var",
    values_to = "difference"
  )

# group difference by kernel and dependent_dist
interpol_comparison_group <- interpol_comparison_with_CVdist %>%
  dplyr::group_by(ds, dt, g, dependent_var) %>%
  dplyr::summarise(
    mean_squared_difference = mean(difference^2),
    .groups = "drop"
  )

# find best kernel
best_kernel <- interpol_comparison_group %>% 
  dplyr::filter(dependent_var %in% c("CVdist2", "CVdist3")) %>% 
  dplyr::group_split(dependent_var) %>%
  purrr::map(function(x) {
    x %>% 
      dplyr::filter(mean_squared_difference == min(mean_squared_difference)) %>%
      magrittr::extract(1,)
  })

save(interpol_comparison, file = "data/parameter_exploration/crossvalidation/interpol_comparison.RData")
save(interpol_comparison_group, file = "data/parameter_exploration/crossvalidation/interpol_comparison_group.RData")
save(best_kernel, file = "data/parameter_exploration/crossvalidation/best_kernel.RData")
