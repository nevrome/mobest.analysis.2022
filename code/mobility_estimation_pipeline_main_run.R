library(magrittr)

#### read parameters ####

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
age_resampling_run <- as.numeric(args[1]) + 1

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final
load("data/spatial/area.RData")
load("data/spatial/mobility_regions.RData")

#### prepare pca model grid ####

number_of_age_resampling_runs <- 3

model_grid_pca <- mobest::create_model_grid(
  independent = list(
      tibble::tibble(
        x = anno$x, 
        y = anno$y, 
        z = sapply(anno$calage_sample, function(x){ x[age_resampling_run] })
      )
    ) %>% stats::setNames(paste0("age_sample_", age_resampling_run)),
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
  independent = list(
    tibble::tibble(
      x = anno$x, 
      y = anno$y, 
      z = sapply(anno$calage_sample, function(x){ x[age_resampling_run] })
    )
  ) %>% stats::setNames(paste0("age_sample_", age_resampling_run)),
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

#### spatial origin ####

interpol_grid_origin <- mobest::search_spatial_origin(interpol_grid)

#### mobility proxy ####

mobility_proxy <- mobest::estimate_mobility(interpol_grid_origin, mobility_regions)

save(mobility_proxy, file = paste0("data/mobility_estimation_main_run/mobility_proxy_", age_resampling_run, ".RData"))

