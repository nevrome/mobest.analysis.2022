library(magrittr)

load("data/gpr/gpr_pred_grid_temporal_sampling_v3.RData")
load("data/gpr/gpr_model_grid_temporal_sampling_v3.RData")
load("data/gpr/prediction_temporal_sampling.RData")

model_grid$prediction_sample <- lapply(prediction, function(x) {
  data.frame(
    point_id = 1:length(x$mean),
    mean = x$mean,
    sd = sqrt(x$s2),
    stringsAsFactors = F
  )
})

model_grid_filtered <- model_grid %>% 
  dplyr::mutate(independent_table_type = ifelse(independent_table_id == "age_center", "age_center", "age_sampled")) %>%
  dplyr::filter(independent_table_type != "age_center", kernel_setting_id == "ds200_dt800_g01")

pred_grid_ind_without_pos <- model_grid_filtered %>%
  dplyr::select(kernel_setting_id, dependent_var_id, independent_table_id, prediction_sample) %>%
  tidyr::unnest(cols = "prediction_sample")

pred_grid_ind <- pred_grid %>%
  dplyr::select(-c("x_01", "y_01", "z_01")) %>%
  dplyr::left_join(
    pred_grid_ind_without_pos, by = "point_id"
  )

save(pred_grid_ind, file = "data/gpr/pred_grid_ind.RData")
