library(magrittr)

patterns <- list.files("data/parameter_exploration/crossvalidation") %>%
  gsub("^interpol_comparison_", "", .) %>%
  gsub("_[0-9]+\\.RData$", "", .) %>%
  unique

# apply function to each pattern
future::plan(future::multisession, workers = 8)
best_kernels <- furrr::future_map_dfr(patterns, function(p) {
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
  # find best kernel
  best_kernel <- cur_interpol_comparison_group %>%
    dplyr::slice_min(order_by = mean_squared_difference, n = 1)
  return(best_kernel)
})

methods_and_fstates <- best_kernels %>%
  dplyr::group_by(multivar_method, multivar_fstate) %>%
  dplyr::summarise(.groups = "drop")

# multivar comparison
purrr::pmap(methods_and_fstates, function(method, fstate) {
  # get best dsx and dsy for this setup
  ? <- best_kernels %>%
    dplyr::filter(multivar_method == method, multivar_fstate == fstate)
  # read and rbind all crossval files for this multivar_method + multivar_fstate with the best kernels
  cur_interpol_comparison <- lapply(
    list.files(
      "data/parameter_exploration/crossvalidation", 
      pattern = paste(method, fstate, sep = "_"),
      full.names = T
    ), function(x) {
      load(x)
      
      
      loaded_dim <- interpol_comparison$dim %>% unique()
      loaded_method <- interpol_comparison$multivar_method %>% unique()
      loaded_fstate <- interpol_comparison$multivar_fstate %>% unique()
      loaded_dsx <- interpol_comparison$dsx %>% unique()
      loaded_dt <- interpol_comparison$dt %>% unique()
      cur_best_kernels %>%
        dplyr::filter(dim == loaded_dim)
      if (cur_best_kernels$dsx
    }
  ) %>% dplyr::bind_rows()
})



save(interpol_comparison, file = "data/parameter_exploration/crossvalidation/interpol_comparison.RData")
save(interpol_comparison_group, file = "data/parameter_exploration/crossvalidation/interpol_comparison_group.RData")
save(best_kernel, file = "data/parameter_exploration/crossvalidation/best_kernel.RData")
