library(magrittr)
load("data/parameter_exploration/distance_products.RData")
source("code/02_parameter_estimation/01a_distance_helper_functions.R")

# fine all patterns of crossvalidation setups
patterns <- list.files("data/parameter_exploration/crossvalidation") %>%
  gsub("^interpol_comparison_", "", .) %>%
  gsub("_[0-9]+\\.RData$", "", .) %>%
  unique

# read and summarise crossvalidation results to one difference measure
# per method, fstate and ds-dt permutation
future::plan(future::multisession, workers = 8)
crossvalidation_kernel_comparison <- furrr::future_map_dfr(patterns, function(p) {
  # read and rbind all crossval output files for this pattern
  cur_interpol_comparison <- lapply(
    list.files(
      "data/parameter_exploration/crossvalidation", 
      pattern = p,
      full.names = T
    ), function(x) {
      load(x)
      interpol_comparison
    }
  ) %>% dplyr::bind_rows()
  # group difference by kernel
  cur_interpol_comparison_group <- cur_interpol_comparison %>%
    dplyr::group_by(dependent_var_id, dim, multivar_method, multivar_fstate, dsx, dsy, dt) %>%
    dplyr::summarise(
      g = unique(g),
      mean_squared_difference = mean(difference^2),
      .groups = "drop"
    )
  return(cur_interpol_comparison_group)
})

save(crossvalidation_kernel_comparison, file = "data/parameter_exploration/crossvalidation_kernel_comparison.RData")

# find best kernel
crossvalidation_best_kernels <- crossvalidation_kernel_comparison %>%
  dplyr::group_by(dependent_var_id, dim, multivar_method, multivar_fstate) %>%
  dplyr::slice_min(order_by = mean_squared_difference, n = 1) %>%
  dplyr::ungroup()

save(crossvalidation_best_kernels, file = "data/parameter_exploration/crossvalidation_best_kernels.RData")

methods_and_fstates <- crossvalidation_best_kernels %>%
  dplyr::group_by(multivar_method, multivar_fstate) %>%
  dplyr::summarise(.groups = "drop")

# read and summarise crossvalidation results to one difference measure
# per method, fstate and multivar multi-dimension space
future::plan(future::multisession, workers = 8)
crossvalidation_multivar_comparison <- furrr::future_pmap_dfr(
  methods_and_fstates,
  function(multivar_method, multivar_fstate) {
    method <- multivar_method
    fstate <- multivar_fstate
    # get best dsx and dsy for this setup
    best_kernel <- crossvalidation_best_kernels %>%
      dplyr::filter(multivar_method == method, multivar_fstate == fstate)
    # read and rbind all crossval files for this multivar_method + multivar_fstate with the best kernels
    cur_interpol_comparison <- lapply(
      list.files(
        "data/parameter_exploration/crossvalidation", 
        pattern = paste(method, fstate, sep = "_"),
        full.names = T
      ), function(x) {
        load(x)
        dplyr::semi_join(
          interpol_comparison,
          best_kernel,
          by = c("dependent_var_id", "dsx", "dt")
        )
      }
    ) %>% dplyr::bind_rows()
    multivar_comparison_raw_long <- cur_interpol_comparison %>%
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
    return(multivar_comparison_grouped)
  }
)

save(crossvalidation_multivar_comparison, file = "data/parameter_exploration/crossvalidation_multivar_comparison.RData")
