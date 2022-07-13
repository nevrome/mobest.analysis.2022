library(magrittr)

patterns <- list.files("data/parameter_exploration/crossvalidation") %>%
  gsub("^interpol_comparison_", "", .) %>%
  gsub("_[0-9]+\\.RData$", "", .) %>%
  unique

# apply function to each pattern
future::plan(future::multisession, workers = 8)
furrr::future_map_dfr(patterns, function(p) {
  # read and rbind all crossvalidation run output files
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
  interpol_comparison_group <- cur_interpol_comparison %>%
    dplyr::group_by(dependent_var_id, dim, multivar_method, multivar_fstate, dsx, dsy, dt) %>%
    dplyr::summarise(
      mean_squared_difference = mean(difference^2),
      .groups = "drop"
    )
  # find best kernel
  best_kernel <- interpol_comparison_group %>%
    dplyr::slice_min(order_by = mean_squared_difference, n = 1)
  
  best_kernel
})


save(interpol_comparison, file = "data/parameter_exploration/crossvalidation/interpol_comparison.RData")
save(interpol_comparison_group, file = "data/parameter_exploration/crossvalidation/interpol_comparison_group.RData")
save(best_kernel, file = "data/parameter_exploration/crossvalidation/best_kernel.RData")
