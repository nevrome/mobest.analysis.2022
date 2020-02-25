library(magrittr)

load("data/gpr/gpr_pred_grid_temporal_sampling_v3.RData")
load("data/gpr/gpr_model_grid_temporal_sampling_v3.RData")
load("data/gpr/prediction_temporal_sampling.RData")

age_center_runs <- which(model_grid$independent_table_id == "age_center")
age_sampled_runs <- which(model_grid$independent_table_id != "age_center")

#### age_center_runs ####
model_grid$prediction_sample[age_center_runs] <- lapply(prediction[age_center_runs], function(x) {
  data.frame(
    point_id = 1:length(x$mean),
    mean = x$mean,
    sd = sqrt(x$s2),
    stringsAsFactors = F
  )
})

prediction_per_point_age_center <- model_grid[age_center_runs,] %>%
  dplyr::select(kernel_setting_id, dependent_var_id, independent_table_id, prediction_sample) %>%
  tidyr::unnest(cols = "prediction_sample")

#### age_sampled_runs ####
#sample from kriging result for each point
prediction_sample_list <- pbapply::pblapply(prediction[age_sampled_runs], function(y) {
  sapply(1:length(y$mean), function(i) { rnorm(1, y$mean[i], sqrt(y$s2[i])) })
}, cl = 8)

# transform to long data.frame
model_grid$prediction_sample <- NA
model_grid$prediction_sample[age_sampled_runs] <- lapply(1:length(prediction_sample_list), function(i) {
  data.frame(
    point_id = 1:length(prediction_sample_list[[i]]),
    pred_samples = prediction_sample_list[[i]],
    stringsAsFactors = F
  )
})

# combine prediction for each kernel, each PC and each run (mean)
prediction_per_point_age_sampled <- model_grid[age_sampled_runs,] %>%
  dplyr::select(kernel_setting_id, dependent_var_id, independent_table_id, prediction_sample) %>%
  tidyr::unnest(cols = "prediction_sample") %>%
  # special treatment of independent_table_id == "age_center"
  dplyr::mutate(independent_table_id = ifelse(independent_table_id == "age_center", "age_center", "age_sampled")) %>%
  dplyr::group_by(independent_table_id, kernel_setting_id, dependent_var_id, point_id) %>%
  dplyr::summarize(mean = mean(pred_samples), sd = sd(pred_samples)) %>%
  dplyr::ungroup()


#### rbind age_center_runs and age_sampled_runs ####
prediction_per_point_df <- rbind(prediction_per_point_age_center, prediction_per_point_age_sampled)

#### add prediction to pred_grid ####
pred_grid <- pred_grid %>%
  dplyr::left_join(
    prediction_per_point_df, by = "point_id"
  )

save(pred_grid, file = "data/gpr/pred_grid.RData")

#### transform pred grid to spatial object ####

pred_grid_spatial <- sf::st_as_sf(pred_grid, coords = c("x_real", "y_real"), crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs") %>%
  dplyr::mutate(
    x_real = sf::st_coordinates(.)[,1],
    y_real = sf::st_coordinates(.)[,2]
  )

#### store results ####

save(pred_grid_spatial, file = "data/gpr/pred_grid_spatial.RData")
