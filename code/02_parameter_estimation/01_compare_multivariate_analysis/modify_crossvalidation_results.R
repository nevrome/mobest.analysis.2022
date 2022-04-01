library(magrittr)

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

# calculate squared Euclidean distances
d <- function(...) {
  in_vecs <- list(...)
  checkmate::assert_true(length(unique(purrr::map_int(in_vecs, length))) == 1)
  purrr::map(in_vecs, function(x) { x^2 }) %>% purrr::reduce(`+`) %>% sqrt
}

d_cum_df <- function(x) {
  purrr::map_dfc(2:ncol(x), function(i) {
    d_in_list <- as.list(x[,1:i])
    dist_vec <- do.call(d, d_in_list)
    setNames(
      list(dist_vec),
      paste0("C1toC", readr::parse_number(colnames(x)[i]))
    )
  }
  )
}

multivar_comparison_raw_long <- multivar_comparison_raw %>%
  tidyr::pivot_wider(
    id_cols = c("id", "mixing_iteration", "multivar_method", "multivar_fstate"),
    names_from = "dependent_var",
    values_from = "difference"
  )

multivar_comparison_with_multidim_dists <- dplyr::bind_cols(
    dplyr::select(
      multivar_comparison_raw_long, 
      -tidyselect::starts_with("C")
    ),
    dplyr::select(
      multivar_comparison_raw_long, 
      -id, -mixing_iteration, -multivar_method, -multivar_fstate
    ) %>% d_cum_df()
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("C"),
    names_to = "dependent_var",
    values_to = "difference"
  )

# group difference by multivar method and and dependent_dist
multivar_comparison_group <- multivar_comparison_with_multidim_dists %>%
  dplyr::group_by(multivar_method, multivar_fstate, dependent_var) %>%
  dplyr::summarise(
    mean_squared_difference = mean(difference^2),
    .groups = "drop"
  )

library(ggplot2)
multivar_comparison_group %>%
  ggplot() +
  geom_point(aes(x = dependent_var, y = mean_squared_difference, color = multivar_method, shape = multivar_fstate))

# # find best kernel
# best_kernel <- interpol_comparison_group %>% 
#   dplyr::filter(
#     dependent_var == "CVdist2" & g == min(g) |
#       dependent_var == "CVdist3" & g == max(g)) %>% 
#   dplyr::group_split(dependent_var, g) %>%
#   purrr::map(function(x) {
#     x %>% 
#       dplyr::filter(mean_squared_difference == min(mean_squared_difference)) %>%
#       magrittr::extract(1,)
#   })
# 
# save(interpol_comparison, file = "data/parameter_exploration/crossvalidation/interpol_comparison.RData")
# save(interpol_comparison_group, file = "data/parameter_exploration/crossvalidation/interpol_comparison_group.RData")
# save(best_kernel, file = "data/parameter_exploration/crossvalidation/best_kernel.RData")
