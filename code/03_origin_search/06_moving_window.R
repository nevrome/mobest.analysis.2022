library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/origin_search/origin_grid_modified.RData")

#### prepare output data ####

# means for search points across age resampling
origin_grid_mean <- mobest::average_origin_searchid(origin_grid_modified)

# moving window
moving_origin_grid <- mobest::average_origin_moving_window(
  origin_grid_modified,
  window_start = -8000,
  window_stop = 2000,
  window_width = 400,
  window_step = 50
)

# no data windows
no_data_windows <- mobest::no_data_windows(moving_origin_grid, window_step = 50)

#### save output ####

save(origin_grid_mean, file = "data/origin_search/origin_grid_mean.RData")
save(moving_origin_grid, file = "data/origin_search/moving_origin_grid.RData")
save(no_data_windows, file = "data/origin_search/no_data_windows.RData")
