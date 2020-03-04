library(magrittr)

#### load data ####

load("data/gpr/gpr_pred_grid_temporal_sampling_v3.RData")
load("data/gpr/gpr_model_grid_temporal_sampling_v3.RData")
load("data/gpr/prediction_temporal_sampling.RData")

#### simplified model_grid ####

model_grid_simplified <- model_grid %>% 
  dplyr::mutate(independent_table_type = ifelse(independent_table_id == "age_center", "age_center", "age_sampled")) %>%
  dplyr::select(-kernel_setting, -independent_table, -dependent_var)

#### add prediction results for each run as a data.frame in a list column to model_grid ####

model_grid_simplified$prediction_sample <- lapply(prediction, function(x) {
  data.frame(
    point_id = 1:length(x$mean),
    mean = x$mean,
    sd = sqrt(x$s2),
    stringsAsFactors = F
  )
})

#### unnest prediction to get a point-wise prediction table ####

pred_grid_filled_without_pos <- model_grid_simplified %>%
  tidyr::unnest(cols = "prediction_sample")

#### merge with pred_grid to add other relevant, spatial information ####

pred_grid_filled <- pred_grid %>%
  dplyr::select(-c("x_01", "y_01", "z_01")) %>%
  dplyr::left_join(
    pred_grid_filled_without_pos, by = "point_id"
  )

#### store result ####

save(pred_grid_filled, file = "data/gpr/pred_grid_filled.RData")

#### transform pred grid to spatial object ####

pred_grid_filled_spatial <- sf::st_as_sf(
  pred_grid_filled, 
  coords = c("x_real", "y_real"), 
  crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
  remove = FALSE
)

#### store results ####

save(pred_grid_filled_spatial, file = "data/gpr/pred_grid_filled_spatial.RData")

#### group all age_sampling runs in pred_grid_filled #### 

age_center_catering_sd <- function(independent_table_type, input_mean, input_sd) {
  if (unique(independent_table_type) == "age_center") {
    input_sd
  } else {
    sd(input_mean)
  }
}

pred_grid_filled_grouped <- pred_grid_filled %>%
  dplyr::group_by(x_real, y_real, age_sample, point_id, independent_table_type, kernel_setting_id, dependent_var_id) %>%
  dplyr::summarize(
    sd = age_center_catering_sd(independent_table_type, mean, sd),
    mean = mean(mean)
  ) %>%
  dplyr::ungroup()

#### store result ####

save(pred_grid_filled_grouped, file = "data/gpr/pred_grid_filled_grouped.RData")

#### transform pred grid to spatial object ####

pred_grid_filled_grouped_spatial <- sf::st_as_sf(
  pred_grid_filled_grouped, 
  coords = c("x_real", "y_real"), 
  crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
  remove = FALSE
)

#### store results ####

save(pred_grid_filled_grouped_spatial, file = "data/gpr/pred_grid_filled_grouped_spatial.RData")
