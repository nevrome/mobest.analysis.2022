library(magrittr)

load("data/gpr/gpr_prep_temporal_sampling_v3.RData")
load("data/gpr/prediction_list_temporal_sampling.RData")

#### sample from kriging result for each point ####
# for every PC
prediction_sample_list <- lapply(prediction_list, function(x) {
  # for every time sampling run
  lapply(x, function(y) {
      pred_sample = sapply(1:length(y$mean), function(i) { rnorm(1, y$mean[i], sqrt(y$s2[i])) })
  })
})

#### transform to long data.frame ####
# for every PC
prediction_sample_df <- lapply(1:length(prediction_sample_list), function(j, prediction_sample_list) {
  x <- prediction_sample_list[[j]]
  # for every time sampling run
  lapply(1:length(x), function(i, x, j) {
    data.frame(
      PC = j,
      run_id = i,
      point_id = 1:length(x[[i]]),
      pred_samples = x[[i]]
    )
  }, x, j) %>% dplyr::bind_rows()
}, prediction_sample_list) %>% dplyr::bind_rows() %>% tibble::as_tibble()

#### combine prediction for each PC and each run (mean) ####

prediction_per_point_df <- prediction_sample_df %>%
  dplyr::group_by(PC, point_id) %>%
  dplyr::summarize(mean = mean(pred_samples), sd = sd(pred_samples))

#### add prediction to pred_grid ####

pred_grid <- pred_grid %>%
  dplyr::mutate(
    pred_PC1_mean = prediction_per_point_df %>% dplyr::filter(PC == 1) %$% mean,
    pred_PC1_sd = prediction_per_point_df %>% dplyr::filter(PC == 1) %$% sd,
    pred_PC2_mean = prediction_per_point_df %>% dplyr::filter(PC == 2) %$% mean,
    pred_PC2_sd = prediction_per_point_df %>% dplyr::filter(PC == 2) %$% sd,
    pred_PC3_mean = prediction_per_point_df %>% dplyr::filter(PC == 3) %$% mean,
    pred_PC3_sd = prediction_per_point_df %>% dplyr::filter(PC == 3) %$% sd,
    pred_PC4_mean = prediction_per_point_df %>% dplyr::filter(PC == 4) %$% mean,
    pred_PC4_sd = prediction_per_point_df %>% dplyr::filter(PC == 4) %$% sd
  )

save(pred_grid, file = "data/gpr/pred_grid_temporal_sampling.RData")

#### transform pred grid to spatial object ####

pred_grid_spatial_cropped <- sf::st_as_sf(pred_grid, coords = c("x_real", "y_real"), crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs") %>%
  dplyr::mutate(
    x_real = sf::st_coordinates(.)[,1],
    y_real = sf::st_coordinates(.)[,2]
  )

#### store results ####

save(pred_grid_spatial_cropped, file = "data/gpr/pred_grid_spatial_cropped_temporal_sampling.RData")
