library(magrittr)

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final
load("data/spatial/area.RData")
load("data/spatial/mobility_regions.RData")

#### prepare pca model grid ####

number_of_age_resampling_runs <- 3

model_grid_pca <- mobest::create_model_grid(
  independent = c(
    list(age_center = tibble::tibble(x = anno$x, y = anno$y, z = anno$calage_center)),
    lapply(
      1:number_of_age_resampling_runs,
      function(i) {
        age_sample <- sapply(anno$calage_sample, function(x){ x[i] })
        tibble::tibble(x = anno$x, y = anno$y, z = age_sample)
      }
    ) %>% stats::setNames(paste0("age_sample_", 1:number_of_age_resampling_runs))
  ),
  dependent = list(
    PC1 = anno$PC1,
    PC2 = anno$PC2,
    PC3 = anno$PC3,
    PC4 = anno$PC4
  ),
  kernel = list(
    ds100_dt100_g001 = list(d = c(100000, 100000, 200), g = 0.01, on_residuals = T, auto = F),
    ds200_dt200_g001 = list(d = c(200000, 200000, 200), g = 0.01, on_residuals = T, auto = F),
    ds500_dt500_g001 = list(d = c(500000, 500000, 500), g = 0.01, on_residuals = T, auto = F),
    ds1000_dt1000_g001 = list(d = c(1000000, 1000000, 1000), g = 0.01, on_residuals = T, auto = F),
    ds2000_dt2000_g001 = list(d = c(2000000, 2000000, 1000), g = 0.01, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    scs100_tl100 = mobest::create_prediction_grid(area, spatial_cell_size = 100000, time_layers = seq(-7500, -500, 100)),
    scs200_tl200 = mobest::create_prediction_grid(area, spatial_cell_size = 500000, time_layers = seq(-7500, -500, 500))
  )
)

#### prepare mds model grid ####

anno_mds <- anno %>% dplyr::filter(
  !is.na(C1)
)

model_grid_mds <- mobest::create_model_grid(
  independent = c(
    list(age_center = tibble::tibble(x = anno_mds$x, y = anno_mds$y, z = anno_mds$calage_center)),
    lapply(
      1:number_of_age_resampling_runs,
      function(i) {
        age_sample <- sapply(anno_mds$calage_sample, function(x){ x[i] })
        tibble::tibble(x = anno_mds$x, y = anno_mds$y, z = age_sample)
      }
    ) %>% stats::setNames(paste0("age_sample_", 1:number_of_age_resampling_runs))
  ),
  dependent = list(
    C1 = anno_mds$C1,
    C2 = anno_mds$C2,
    C3 = anno_mds$C3,
    C4 = anno_mds$C4
  ),
  kernel = list(
    ds100_dt100_g001 = list(d = c(100000, 100000, 200), g = 0.01, on_residuals = T, auto = F),
    ds200_dt200_g001 = list(d = c(200000, 200000, 200), g = 0.01, on_residuals = T, auto = F),
    ds500_dt500_g001 = list(d = c(500000, 500000, 500), g = 0.01, on_residuals = T, auto = F),
    ds1000_dt1000_g001 = list(d = c(1000000, 1000000, 1000), g = 0.01, on_residuals = T, auto = F),
    ds2000_dt2000_g001 = list(d = c(2000000, 2000000, 1000), g = 0.01, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    scs100_tl100 = mobest::create_prediction_grid(area, spatial_cell_size = 100000, time_layers = seq(-7500, -500, 100)),
    scs200_tl200 = mobest::create_prediction_grid(area, spatial_cell_size = 500000, time_layers = seq(-7500, -500, 500))
  )
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

