library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")
load("data/spatial/mobility_regions.RData")

#### prepare pca model grid ####
model_grid <- mobest::create_model_grid(
  independent = list(
    age_median = mobest::create_spatpos(
      id = janno_final$Individual_ID,
      x = janno_final$x,
      y = janno_final$y,
      z = janno_final$Date_BC_AD_Median_Derived
    )
  ),
  dependent = list(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  kernel = list(
    ds450_dt800_g006 = mobest::create_kernset(
      d = c(450000, 450000, 800), 
      g = 0.06, 
      on_residuals = T, 
      auto = F
    )
  ),
  prediction_grid = list(
    scs100_tl100 = mobest::create_prediction_grid(
      area,
      mobility_regions,
      spatial_cell_size = 100000,
      time_layers = seq(-7800, 1500, 50)
    )
  )
)

save(interpol_grid, file = "data/gpr/model_grid_median.RData")

#### run interpolation on model grid ####

interpol_grid <- mobest::run_model_grid(model_grid)

# library(ggplot2)
# interpol_grid %>%
#   dplyr::filter(
#     dependent_var_id == "C1",
#     z %in% seq(-7000, -1000, 500)
#   ) %>%
#   ggplot() +
#   geom_raster(aes(x, y, fill = mean, alpha = sd)) +
#   facet_wrap(~z) +
#   scale_fill_viridis_c() +
#   scale_alpha_continuous(range = c(1, 0), na.value = 0)

save(interpol_grid, file = "data/gpr/interpol_grid_median.RData")
