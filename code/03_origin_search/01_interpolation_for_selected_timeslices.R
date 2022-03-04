library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/extended_area.RData")
load("data/origin_search/default_kernel.RData")

#### prepare model grid ####

model_grid <- mobest::create_model_grid(
  independent = mobest::create_spatpos_multi(
    id = janno_final$Poseidon_ID,
    x = list(janno_final$x),
    y = list(janno_final$y),
    z = list(janno_final$Date_BC_AD_Median_Derived),
    it = "age_median"
  ),
  dependent = mobest::create_obs(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  kernel = default_kernel,
  prediction_grid = list(
    scs100_tl50 = mobest::prediction_grid_for_spatiotemporal_area(
      extended_area,
      spatial_cell_size = 50000,
      temporal_layers = seq(-7000, 1000, 2000)
    )
  )
)

#### run interpolation on model grid ####

interpol_grid <- mobest::run_model_grid(model_grid)

save(interpol_grid, file = "data/gpr/interpol_grid_median_selected_timeslices.RData")
