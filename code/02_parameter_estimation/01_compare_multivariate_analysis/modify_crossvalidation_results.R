library(magrittr)

load("data/parameter_exploration/multivariate_analysis_comparison/distance_products.RData")
source("code/02_parameter_estimation/01_compare_multivariate_analysis/distance_helper_functions.R")

# read and rbind all crossvalidation run output files
multivar_comparison_raw <- lapply(
  list.files(
    "data/parameter_exploration/multivariate_analysis_comparison/crossvalidation/", 
    pattern = "multivar_comparison_[0-9]+",
    full.names = T
  ), function(x) {
    load(x)
    multivar_comparison
  }
) %>% dplyr::bind_rows()

multivar_comparison_raw_long <- multivar_comparison_raw %>%
  tidyr::pivot_wider(
    id_cols = c("id", "mixing_iteration", "multivar_method", "multivar_fstate"),
    names_from = "dependent_var_id",
    values_from = "difference"
  )

# calculate multidim differences
multivar_comparison_with_multidim_dists <- dplyr::bind_cols(
    dplyr::select(
      multivar_comparison_raw_long, 
      -tidyselect::starts_with("C")
    ),
    dplyr::select(
      multivar_comparison_raw_long, 
      -id, -mixing_iteration, -multivar_method, -multivar_fstate
    ) %>%
      d_cum_df()
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("C"),
    names_to = "dependent_var",
    values_to = "difference"
  )

# scale multidim differences with the mean pairwise distance in multidim space
multivar_comparison_multidim_scaled <- multivar_comparison_with_multidim_dists %>%
  dplyr::left_join(
    distance_products,
    by = c(
      "multivar_method" = "method",
      "multivar_fstate" = "snp_selection",
      "dependent_var" = "dim_range"
    )
  ) %>%
  dplyr::mutate(
    scaled_difference = difference / mean_pairwise_distances
  )

# group difference by multivar method and and dependent_dist
multivar_comparison_grouped <- multivar_comparison_multidim_scaled %>%
  dplyr::group_by(multivar_method, multivar_fstate, dependent_var) %>%
  dplyr::summarise(
    mean_squared_difference_estimated_real = mean(scaled_difference^2),
    .groups = "drop"
  )

multivar_comparison_summary <- distance_products %>%
  dplyr::left_join(
    multivar_comparison_grouped,
    by = c(
      "dim_range" = "dependent_var",
      "method" = "multivar_method",
      "snp_selection" = "multivar_fstate"
    )
  )

save(
  multivar_comparison_summary,
  file = "data/parameter_exploration/multivariate_analysis_comparison/multivar_comparison_summary.RData"
)
