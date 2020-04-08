library(magrittr)

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered
load("data/spatial/area.RData")
load("data/spatial/mobility_regions.RData")

#### split ####

anno_mixed <- anno[sample(1:nrow(anno), replace = F), ]

n <- 10
nr <- nrow(anno_mixed)
anno_10 <- split(anno_mixed, rep(1:n, times = diff(floor(seq(0, nr, length.out = n + 1)))))

anno_9_training <- lapply(
  1:n, function(i) {
    dplyr::bind_rows(anno_10[-i])
  }
)

anno_9_test <- lapply(
  1:n, function(i) {
    anno_10[[i]]
  }
)

#### prepare model grid ####

# prep independent variables with temporal sampling
independent_tables <- tibble::tibble(
  independent_table = lapply(
    anno_9_training, function(x) { dplyr::transmute(.data = x, x = x, y = y, z = calage_center) }
  ),
  independent_table_id = paste0("age_center_", 1:(length(independent_table)))
)

# prep dependent vars
dependent_vars <- tibble::tibble(
  dependent_var_id = c("PC1", "PC2", "PC3", "PC4")
) %>%
  dplyr::mutate(
    dependent_var = lapply(dependent_var_id, function(x) { anno[[x]] })
  )

# create kernel parameters
kernel_settings <- tibble::tibble(
  kernel_setting = list(
    #ds50_dt100_g01 = list(auto = F, d = c(dist_scale_01_x_km(50), dist_scale_01_x_km(50), dist_scale_01_z_years(100)), g = 0.1),
    #ds100_dt200_g01 = list(auto = F, d = c(dist_scale_01_x_km(100), dist_scale_01_x_km(100), dist_scale_01_z_years(200)), g = 0.1),
    ds200_dt400_g01 = list(auto = F, d = c(200000, 200000, 400), g = 0.1)
  ), 
  kernel_setting_id = names(kernel_setting)
)

# create spatiotemporal prediction grid
pred_grids <- tibble::tibble(
  pred_grid = list(
    scs100_tl100 = mobest::create_prediction_grid(area, spatial_cell_size = 100000, time_layers = seq(-7500, -500, 100)),
    scs200_tl200 = mobest::create_prediction_grid(area, spatial_cell_size = 200000, time_layers = seq(-7500, -500, 200))
  ),
  pred_grid_id = names(pred_grid)
)

# merge info in prepare model grid
model_grid <- mobest::create_model_grid(
  independent_tables = independent_tables, 
  dependent_vars = dependent_vars,
  kernel_settings = kernel_settings,
  pred_grids = pred_grids
)