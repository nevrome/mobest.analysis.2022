library(magrittr)

# read and rbind all crossvalidation run output files
interpol_comparison <- lapply(
  list.files(
    "data/parameter_exploration/crossvalidation", 
    pattern = "interpol_comparison_[0-9]+",
    full.names = T
  ), function(x) {
    load(x)
    interpol_comparison
  }
) %>% dplyr::bind_rows()

# interpol_comparison <- dplyr::slice_sample(interpol_comparison, n = 100000)

# group difference by kernel and dependent_dist
interpol_comparison_group <- interpol_comparison %>%
  dplyr::group_by(dsx, dsy, dt, dependent_var_id) %>%
  dplyr::summarise(
    mean_squared_difference = mean(difference^2),
    .groups = "drop"
  )

# find best kernel
best_kernel <- interpol_comparison_group %>%
  dplyr::group_by(dependent_var_id) %>%
  dplyr::slice_min(order_by = mean_squared_difference, n = 1) %>%
  dplyr::ungroup()

save(interpol_comparison, file = "data/parameter_exploration/crossvalidation/interpol_comparison.RData")
save(interpol_comparison_group, file = "data/parameter_exploration/crossvalidation/interpol_comparison_group.RData")
save(best_kernel, file = "data/parameter_exploration/crossvalidation/best_kernel.RData")
