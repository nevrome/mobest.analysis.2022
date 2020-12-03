# sbatch code/mobility_estimation/slurm_05_mobility_estimation_pipeline_age_resampling.sh

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")
load("data/spatial/mobility_regions.RData")

#### prepare pca model grid ####
model_grid <- mobest::create_model_grid(
  independent = stats::setNames(list(
    tibble::tibble(
      x = janno_final$x, 
      y = janno_final$y, 
      z = janno_final$Date_BC_AD_Median_Derived
    )
  ), "age_median"),
  dependent = list(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  kernel = list(
    ds450_dt800_g006 = list(d = c(450000, 450000, 800), g = 0.06, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    scs100_tl100 = mobest::create_prediction_grid(
      area, 
      mobility_regions,
      spatial_cell_size = 100000, 
      time_layers = seq(-8000, 1500, 50)
    )
  )
)

#### run interpolation on model grid ####

interpol_grid <- mobest::run_model_grid(model_grid)

#### spatial origin ####

interpol_grid_origin_1 <- mobest::search_spatial_origin(interpol_grid, steps = 2)
interpol_grid_origin_2 <- mobest::search_spatial_origin(interpol_grid, steps = 6)
interpol_grid_origin_3 <- mobest::search_spatial_origin(interpol_grid, steps = 10)

#### mobility proxy ####

mobility_proxy_1 <- mobest::estimate_mobility(interpol_grid_origin_1) %>% dplyr::mutate(steps = "100y")
mobility_proxy_2 <- mobest::estimate_mobility(interpol_grid_origin_2) %>% dplyr::mutate(steps = "300y")
mobility_proxy_3 <- mobest::estimate_mobility(interpol_grid_origin_3) %>% dplyr::mutate(steps = "500y")

mobility_proxy <- rbind(mobility_proxy_1, mobility_proxy_2, mobility_proxy_3)

save(
  mobility_proxy, 
  file = "data/mobility_estimation/mean_age+origin_search(+multiple_step_settings)+directed_speed+one_kernel_setting/run.RData"
)

# scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/mobility_estimation/age_resampling/* .
