library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/origin_search/retrospection_distance_retrovar.RData")

#### load, merge and modify data ####

# load resampling runs
origin_grid_superlist <- lapply(
  list.files(
    "data/origin_search/age_resampling+one_kernel_setting", 
    pattern = "retrovar_run_[0-9]", 
    full.names = T
  ), function(x) {
    load(x)
    origin_grid_list
  }
) %>%
  purrr::transpose()

purrr::walk2(
  origin_grid_superlist,
  names(retrospection_distances),
  function(origin_grid_list, retrovar_setting_name) {
  
  origin_grid <- origin_grid_list %>% dplyr::bind_rows()
  
  # join with janno_final 
  origin_grid_modified <- origin_grid %>% 
    dplyr::mutate(
      spatial_distance = spatial_distance/1000
    ) %>%
    dplyr::left_join(
      janno_final %>% dplyr::select(Poseidon_ID, region_id),
      by = c("search_id" = "Poseidon_ID")
    )
  
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
  origin_grid_derived_data <- list(
    origin_grid_modified = origin_grid_modified,
    origin_grid_mean = origin_grid_mean,
    moving_origin_grid = moving_origin_grid,
    no_data_windows = no_data_windows
  )
  
  save(
    origin_grid_derived_data, 
    file = paste0(
      "data/origin_search/origin_grid_derived_data_retro_",  
      retrovar_setting_name,
      ".RData"
    )
  )

})
