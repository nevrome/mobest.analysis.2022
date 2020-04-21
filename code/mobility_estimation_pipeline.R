library(magrittr)

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final
load("data/spatial/area.RData")
load("data/spatial/mobility_regions.RData")

#### prepare pca model grid ####

# individual point
sf::st_as_sf(
  tibble::tibble(lon = 9.05, lat = 48.52),
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
) %>%
  sf::st_transform(
    crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
  ) %>% sf::st_coordinates()


mobest::create_model_grid(
  independent = c(
    list(age_center = tibble::tibble(x = anno$x, y = anno$y, z = anno$calage_center))#,
    # lapply(
    #   1:50,
    #   function(i) {
    #     age_sample <- sapply(anno$calage_sample, function(x){ x[i] })
    #     tibble::tibble(x = anno$x, y = anno$y, z = age_sample)
    #   }
    # ) %>% stats::setNames(paste0("age_sample_", 1:50))
  ),
  dependent = list(
    PC1 = anno$PC1,
    PC2 = anno$PC2,
    PC3 = anno$PC3,
    PC4 = anno$PC4
  ),
  kernel = list(
    #ds50_dt100_g01 = list(auto = F, d = c(dist_scale_01_x_km(50), dist_scale_01_x_km(50), dist_scale_01_z_years(100)), g = 0.1),
    #ds100_dt200_g01 = list(auto = F, d = c(dist_scale_01_x_km(100), dist_scale_01_x_km(100), dist_scale_01_z_years(200)), g = 0.1),
    #ds500_dt500_g01 = list(d = c(500000, 500000, 500), g = 0.1, on_residuals = T, auto = F),
    ds1000_dt1000_g01 = list(d = c(1000000, 1000000, 1000), g = 0.1, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    #scs100_tl100 = mobest::create_prediction_grid(area, spatial_cell_size = 100000, time_layers = seq(-7500, -500, 100)),
    #scs200_tl200 = mobest::create_prediction_grid(area, spatial_cell_size = 200000, time_layers = seq(-7500, -500, 200))
    tuebingen = tibble::tibble(x = -69459.46, y = 2031623, z = seq(-7500, -500, 100), point_id = 1:71)
  )
)

#### prepare mds model grid ####

anno_mds <- anno %>% dplyr::filter(
  !is.na(C1)
)

# prep independent variables with temporal sampling
independent_tables <- tibble::tibble(
  independent_table = c(
    list(dplyr::transmute(.data = anno_mds, x = x, y = y, z = calage_center)) 
  ),
  independent_table_id = c("age_center")
)

# prep dependent vars
dependent_vars <- tibble::tibble(
  dependent_var_id = c("C1", "C2", "C3", "C4")
) %>%
  dplyr::mutate(
    dependent_var = lapply(dependent_var_id, function(x) { anno_mds[[x]] })
  )

# merge info in prepare model grid
model_grid_mds <- mobest::create_model_grid(
  independent_tables = independent_tables, 
  dependent_vars = dependent_vars,
  kernel_settings = kernel_settings,
  pred_grids = pred_grids
)

#### merge model grids ####

model_grid <- rbind(model_grid_pca, model_grid_mds)

#### run interpolation on model grid ####

model_grid_result <- mobest::run_model_grid(model_grid)

#### unnest prediction to get a point-wise prediction table ####

interpol_grid <- mobest::unnest_model_grid(model_grid_result)

save(interpol_grid, file = "data/gpr/interpol_grid.RData")

interpol_grid_spatial <- sf::st_as_sf(
  interpol_grid,
  coords = c("x", "y"),
  crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
  remove = FALSE
)
save(interpol_grid_spatial, file = "data/gpr/interpol_grid_spatial.RData")

library(ggplot2)
interpol_grid_spatial %>%
  dplyr::filter(
    independent_table_id == "age_center",
    dependent_var_id == "C1",
    kernel_setting_id == "ds500_dt500_g01",
    pred_grid_id == "scs100_tl100",
    z %% 500 == 0
  ) %>%
  ggplot() +
  geom_raster(aes(x, y, fill = mean)) +
  facet_wrap(~z) +
  scale_fill_viridis_c()


#### group all age_sampling runs in interpol_grid #### 

interpol_grid_condensed <- mobest::condense_interpol_grid(interpol_grid)

save(interpol_grid_condensed, file = "data/gpr/interpol_grid_condensed.RData")

interpol_grid_condensed_spatial <- sf::st_as_sf(
  interpol_grid_condensed,
  coords = c("x", "y"),
  crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
  remove = FALSE
)
save(interpol_grid_condensed_spatial, file = "data/gpr/interpol_grid_condensed_spatial.RData")


#### spatial origin ####

interpol_grid_origin <- mobest::search_spatial_origin(interpol_grid)

save(interpol_grid_origin, file = "data/interpol_grid_origin.RData")

interpol_grid_origin_spatial <- sf::st_as_sf(
  interpol_grid_origin,
  coords = c("x", "y"),
  crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
  remove = F
)
save(interpol_grid_origin_spatial, file = "data/interpol_grid_origin_spatial.RData")

#### mobility proxy ####

mobility_proxy <- mobest::estimate_mobility(interpol_grid_origin, mobility_regions)

save(mobility_proxy, file = "data/mobility_estimation/mobility_proxy.RData")

