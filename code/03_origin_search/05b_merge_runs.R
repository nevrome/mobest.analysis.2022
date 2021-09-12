library(magrittr)

#### load, merge and modify data ####

# load resampling runs
origin_grid <- lapply(
  list.files(
    "data/origin_search/age_resampling+one_kernel_setting", 
    pattern = "run_[0-9]", 
    full.names = T
  ), function(x) {
    load(x)
    origin_grid
  }
) %>% dplyr::bind_rows()

# join with janno_final 
origin_grid_modified <- origin_grid %>% 
  dplyr::mutate(
    spatial_distance = spatial_distance/1000
  ) %>%
  dplyr::left_join(
    janno_final %>% dplyr::select(Individual_ID, region_id),
    by = c("search_id" = "Individual_ID")
  )

# define age groups
age_groups_limits <- seq(-7250, 1750, 500)

origin_grid_modified <- origin_grid_modified %>%
  dplyr::mutate(
    search_z_cut = age_groups_limits[cut(search_z, age_groups_limits, labels = F)] + 250,
  )

#### save output ####

save(origin_grid_modified, file = "data/origin_search/origin_grid_modified.RData")
