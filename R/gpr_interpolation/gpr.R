library(magrittr)

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/area.RData")

#### prepare model grid ####
# prep independent variables with temporal sampling
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

# create spatiotemporal prediction grid
pred_grid <- mobest::create_prediction_grid(
  area, 
  spatial_cell_size = 100000,
  time_layers = seq(-7500, -500, 100)
)

# create kernel parameters
kernel_settings <- tibble::tibble(kernel_setting = list(
    #ds50_dt100_g01 = list(auto = F, d = c(dist_scale_01_x_km(50), dist_scale_01_x_km(50), dist_scale_01_z_years(100)), g = 0.1),
    #ds100_dt200_g01 = list(auto = F, d = c(dist_scale_01_x_km(100), dist_scale_01_x_km(100), dist_scale_01_z_years(200)), g = 0.1),
    ds200_dt400_g01 = list(auto = F, d = c(200000, 200000, 400), g = 0.1)
  ), kernel_setting_id = names(kernel_setting))

# merge info in prepare model grid
model_grid <- mobest::create_model_grid(
  independent_tables = independent_tables, 
  dependent_vars = c("PC1", "PC2", "PC3", "PC4"), 
  kernel_settings = kernel_settings
)

#### run interpolation on model grid ####

model_grid_result <- mobest::run_model_grid(model_grid, pred_grid)

#### unnest prediction to get a point-wise prediction table ####

interpol_grid <- mobest::unnest_model_grid(model_grid_result)

save(interpol_grid, file = "data/gpr/interpol_grid.RData")

sf::st_as_sf(
  interpol_grid, 
  coords = c("x", "y"), 
  crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
  remove = FALSE
) %>% 
  save(file = "data/gpr/interpol_grid_spatial.RData")

#### group all age_sampling runs in interpol_grid #### 

interpol_grid_condensed <- mobest::condense_interpol_grid(interpol_grid)

save(interpol_grid_condensed, file = "data/gpr/interpol_grid_condensed.RData")

sf::st_as_sf(
  interpol_grid_condensed, 
  coords = c("x", "y"), 
  crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
  remove = FALSE
) %>%
  save(file = "data/gpr/interpol_grid_condensed_spatial.RData")

