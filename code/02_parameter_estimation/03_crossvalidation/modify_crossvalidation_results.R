library(magrittr)

# read and rbind all crossvalidation run output files
interpol_comparison_raw <- lapply(
  list.files(
    "data/parameter_exploration/crossvalidation", 
    pattern = "interpol_comparison_[0-9]+",
    full.names = T
  ), function(x) {
    load(x)
    interpol_comparison
  }
) %>% dplyr::bind_rows()

# interpol_comparison_raw <- dplyr::slice_sample(interpol_comparison_raw, n = 100000)

# calculate squared Euclidean distances
interpol_comparison_with_CVdist <- interpol_comparison_raw %>%
  tidyr::pivot_wider(
    id_cols = c("id", "mixing_iteration", "dsx", "dsy", "dt"),
    names_from = "dependent_var_id",
    values_from = "difference",
    names_prefix = "CV_diff_"
  ) %>%
  dplyr::mutate(
    CVdiff2 = sqrt(CV_diff_C1_mds_u^2 + CV_diff_C2_mds_u^2),
    CVdiff3 = sqrt(CV_diff_C1_mds_u^2 + CV_diff_C2_mds_u^2 + CV_diff_C3_mds_u^2)
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("C"),
    names_to = "dependent_var",
    values_to = "difference"
  )

# group difference by kernel and dependent_dist
interpol_comparison_group <- interpol_comparison_with_CVdist %>%
  dplyr::group_by(dsx, dsy, dt, dependent_var) %>%
  dplyr::summarise(
    mean_squared_difference = mean(difference^2),
    .groups = "drop"
  )

# find best kernel
best_kernel <- interpol_comparison_group %>% 
  dplyr::filter(
    dependent_var == "CVdiff2" |
      dependent_var == "CVdiff3") %>% 
  dplyr::group_by(dependent_var) %>%
  dplyr::slice_min(order_by = mean_squared_difference, n = 1)

save(interpol_comparison, file = "data/parameter_exploration/crossvalidation/interpol_comparison.RData")
save(interpol_comparison_group, file = "data/parameter_exploration/crossvalidation/interpol_comparison_group.RData")
save(best_kernel, file = "data/parameter_exploration/crossvalidation/best_kernel.RData")
