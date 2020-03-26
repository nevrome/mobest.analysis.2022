library(magrittr)

#### load data ####

load("data/gpr/model_grid_simplified.RData")

#### unnest prediction to get a point-wise prediction table ####

pred_grid_filled <- model_grid_simplified %>%
  tidyr::unnest(cols = "prediction_sample")

#### store result ####

save(pred_grid_filled, file = "data/gpr/pred_grid_filled.RData")

#### transform pred grid to spatial object ####

pred_grid_filled_spatial <- sf::st_as_sf(
  pred_grid_filled, 
  coords = c("x", "y"), 
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
    #sd(input_mean)
    sd(sapply(1:length(input_mean), function(i) { rnorm(1, input_mean[i], input_sd[i]) }))
  }
}

pred_grid_filled_grouped <- pred_grid_filled %>%
  dplyr::group_by(x, y, z, point_id, independent_table_type, kernel_setting_id, dependent_var_id) %>%
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
  coords = c("x", "y"), 
  crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
  remove = FALSE
)

#### store results ####

save(pred_grid_filled_grouped_spatial, file = "data/gpr/pred_grid_filled_grouped_spatial.RData")
