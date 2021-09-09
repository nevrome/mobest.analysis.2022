library(magrittr)

origin_grid <- lapply(
  list.files(
    "data/origin_search/age_resampling+one_kernel_setting", 
    pattern = "mds3_run_[0-9]", 
    full.names = T
  ), function(x) {
    load(x)
    origin_grid
  }
) %>% dplyr::bind_rows()

save(origin_grid, file = "data/origin_search/age_resampling+one_kernel_setting/origin_grid_mds3.RData")

