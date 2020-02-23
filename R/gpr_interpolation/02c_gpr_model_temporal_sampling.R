library(magrittr)

load("data/gpr/gpr_prep_temporal_sampling_v3.RData")
load("data/gpr/prediction_list_temporal_sampling.RData")

#### sample from kriging result for each point ####
# for every kernel setting
prediction_sample_list <- lapply(prediction_list, function(z) {
  # for every PC
  prediction_sample_list <- lapply(z, function(x) {
    # for every time sampling run
    lapply(x, function(y) {
        sapply(1:length(y$mean), function(i) { rnorm(1, y$mean[i], sqrt(y$s2[i])) })
    })
  })
})

#### transform to long data.frame ####
# for every kernel setting
prediction_sample_df <- lapply(names(prediction_sample_list), function(z) {
  # for every PC
  lapply(names(prediction_sample_list[[z]]), function(x) {
    # for every time sampling run
    lapply(1:length(prediction_sample_list[[z]][[x]]), function(i) {
      data.frame(
        kernel = z,
        PC = x,
        run_id = i,
        point_id = 1:length(prediction_sample_list[[z]][[x]][[i]]),
        pred_samples = prediction_sample_list[[z]][[x]][[i]],
        stringsAsFactors = F
      )
    }) %>% dplyr::bind_rows()
  }) %>% dplyr::bind_rows()
}) %>% dplyr::bind_rows() %>% tibble::as_tibble()

#### combine prediction for each kernel, each PC and each run (mean) ####

prediction_per_point_df <- prediction_sample_df %>%
  dplyr::group_by(kernel, PC, point_id) %>%
  dplyr::summarize(mean = mean(pred_samples), sd = sd(pred_samples))

#### add prediction to pred_grid ####
pred_grid <- pred_grid %>%
  dplyr::left_join(
    prediction_per_point_df, by = "point_id"
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
