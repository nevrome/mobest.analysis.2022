library(magrittr)
library(laGP)
source("R/helper_functions.R")

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/area.RData")
bb <- unname(sf::st_bbox(research_area))

#### prep independent variables with temporal sampling ####

independent_tables <- tibble::tibble(
  independent_table = lapply(
    1:2,#1:length(anno$calage_sample[[1]]), 
    function(i, anno) {
      age_sample <- sapply(anno$calage_sample, function(x){ x[i] })
      dplyr::transmute(
        .data = anno,
        x_01 = range_01_x(x),
        y_01 = range_01_y(y),
        z_01 = range_01_z(age_sample)
      )
    },
    anno
  ),
  independent_table_id = 1:length(independent_table)
)
  
  

#### create prediction grid ####

pred_points_space <- area %>% 
  sf::st_make_grid(cellsize = 100000, what = "centers") %>%
  sf::st_intersection(area) %>%
  sf::st_coordinates() %>%
  tibble::as_tibble() %>%
  dplyr::rename(x_real = X, y_real = Y)

time_layers <- tibble::tibble(
  age_sample = seq(-7500, -500, 500)
)

pred_grid <- pred_points_space %>% 
  tidyr::crossing(time_layers) %>%
  dplyr::mutate(
    point_id = 1:nrow(.),
    x_01 = range_01_x(x_real),
    y_01 = range_01_y(y_real),
    z_01 = range_01_z(age_sample)
  )

#### create kernel parameters ####

kernel_settings <- tibble::tibble(
  kernel_setting = list(
    A = list(auto = F, d = c(dist_scale_01_x_km(50), dist_scale_01_x_km(50), dist_scale_01_z_y(200)), g = 0.1),
    B = list(auto = F, d = c(dist_scale_01_x_km(200), dist_scale_01_x_km(200), dist_scale_01_z_y(800)), g = 0.1)#,
    #C = list(auto = T, d = NA, g = NA)
  ),
  kernel_setting_id = LETTERS[1:length(kernel_setting)]
)

#### prepare model grid ####

model_grid <- expand.grid(
  kernel_setting_id = kernel_settings$kernel_setting_id,
  dependent_var_id = c("PC1", "PC2"),
  independent_table_id = independent_tables$independent_table_id,
  stringsAsFactors = F
) %>%
  dplyr::left_join(
    kernel_settings, by = "kernel_setting_id"
  ) %>%
  dplyr::left_join(
    independent_tables, by = "independent_table_id"
  ) %>% dplyr::mutate(
    dependent_var = lapply(dependent_var_id, function(x) { anno[[x]] })
  )


save.image(file = "data/gpr/gpr_prep_temporal_sampling_v2.RData", version = 2)
save.image(file = "data/gpr/gpr_prep_temporal_sampling_v3.RData", version = 3)
