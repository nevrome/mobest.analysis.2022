library(magrittr)

#### data ####

load("data/genotype_data/janno_final.RData")

#### load, merge and modify data ####

# load resampling runs
origin_grid <- lapply(
  list.files(
    "data/origin_search/age_resampling+one_kernel_setting", 
    pattern = "^run_[0-9]+",
    full.names = T
  ), function(x) {
    load(x)
    origin_grid
  }
) %>% dplyr::bind_rows()

# join with janno_final 
# origin_grid_modified <- origin_grid %>% 
#   dplyr::mutate(
#     spatial_distance = spatial_distance/1000
#   ) %>%
#   dplyr::left_join(
#     janno_final %>% dplyr::select(Poseidon_ID, region_id),
#     by = c("search_id" = "Poseidon_ID")
#   )

# define age groups
# age_groups_limits <- seq(-7250, 1750, 500)
# 
# origin_grid_modified <- origin_grid_modified %>%
#   dplyr::mutate(
#     search_z_cut = age_groups_limits[cut(search_z, age_groups_limits, labels = F)] + 250,
#   )

prob_product <- mobest::multiply_dependent_probabilities(origin_grid)

ov <- mobest::determine_origin_vectors(prob_product, independent_table_id)

ov_with_regions <- dplyr::left_join(
  ov,
  janno_final %>% dplyr::select(Poseidon_ID, region_id),
  by = c("search_id" = "Poseidon_ID")
)

# moving window

origin_summary <- mobest::summarize_origin_vectors(
  ov_with_regions,
  region_id,
  window_start = -8000,
  window_stop = 2000,
  window_width = 400,
  window_step = 50
)

# no data windows
no_data_windows <- mobest::find_no_data_windows(origin_summary, region_id)

library(ggplot2)

origin_summary %>%
  ggplot() +
  geom_point(
    aes(z, undirected_mean_spatial_distance)
  ) +
  facet_wrap(~region_id)

#### save output ####

# save(origin_grid_modified, file = "data/origin_search/origin_grid_modified.RData")
# save(origin_grid_mean, file = "data/origin_search/origin_grid_mean.RData")
# save(moving_origin_grid, file = "data/origin_search/moving_origin_grid.RData")
# save(no_data_windows, file = "data/origin_search/no_data_windows.RData")
