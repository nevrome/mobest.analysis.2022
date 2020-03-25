library(magrittr)

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/area.RData")

#### prep independent variables with temporal sampling ####

number_of_age_samples <- 1#50 #max: length(anno$calage_sample[[1]])
independent_tables <- tibble::tibble(
  independent_table = c(
    list(dplyr::transmute(.data = anno, x = x, y = y, z = calage_center)), 
    lapply(
      1:number_of_age_samples, 
      function(i, anno) {
        age_sample <- sapply(anno$calage_sample, function(x){ x[i] })
        dplyr::transmute(.data = anno, x = x, y = y, z = age_sample)
      },
      anno
    )
  ),
  independent_table_id = c("age_center", paste0("age_sample_", 1:(length(independent_table) - 1)))
)

#### create spatial prediction grid ####

pred_grid <- mobest::create_prediction_grid(area, spatial_cell_size = 100000, time_layers = seq(-7500, -500, 100))

#### create kernel parameters ####

kernel_settings <- tibble::tibble(
  kernel_setting = list(
    #ds50_dt100_g01 = list(auto = F, d = c(dist_scale_01_x_km(50), dist_scale_01_x_km(50), dist_scale_01_z_years(100)), g = 0.1),
    #ds100_dt200_g01 = list(auto = F, d = c(dist_scale_01_x_km(100), dist_scale_01_x_km(100), dist_scale_01_z_years(200)), g = 0.1),
    ds200_dt400_g01 = list(auto = F, d = c(200000, 200000, 400), g = 0.1)
  ),
  kernel_setting_id = names(kernel_setting)
)

#### prepare model grid ####

model_grid <- expand.grid(
  kernel_setting_id = kernel_settings$kernel_setting_id,
  dependent_var_id = c("PC1", "PC2", "PC3", "PC4"),
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
  ) %>% tibble::as_tibble()

#### store intermediate data ###

save(pred_grid, file = "data/gpr/gpr_pred_grid_temporal_sampling.RData")
save(model_grid, file = "data/gpr/gpr_model_grid_temporal_sampling.RData")
