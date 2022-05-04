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

# interpol_comparison_raw <- dplyr::slice_sample(interpol_comparison_raw, n = 100000)

# calculate squared Euclidean distances
interpol_comparison_with_CVdist <- interpol_comparison %>%
  tidyr::pivot_wider(
    id_cols = c("id", "mixing_iteration", "dsx", "dsy", "dt"),
    names_from = "dependent_var_id",
    values_from = "difference",
    names_prefix = "CVdiff_"
  ) %>%
  dplyr::mutate(
    CVdiff_summary_2D = sqrt(CVdiff_C1_mds_u^2 + CVdiff_C2_mds_u^2),
    CVdiff_summary_3D = sqrt(CVdiff_C1_mds_u^2 + CVdiff_C2_mds_u^2 + CVdiff_C3_mds_u^2)
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
  dplyr::group_by(dependent_var) %>%
  dplyr::slice_min(order_by = mean_squared_difference, n = 1)

save(interpol_comparison, file = "data/parameter_exploration/crossvalidation/interpol_comparison.RData")
save(interpol_comparison_group, file = "data/parameter_exploration/crossvalidation/interpol_comparison_group.RData")
save(best_kernel, file = "data/parameter_exploration/crossvalidation/best_kernel.RData")
