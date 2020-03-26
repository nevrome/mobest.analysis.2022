load("data/gpr/gpr_pred_grid_temporal_sampling.RData")
load("data/gpr/gpr_model_grid_temporal_sampling.RData")

model_grid_simplified <- mobest::run_model_grid(model_grid, pred_grid)

save(model_grid_simplified, file = "data/gpr/model_grid_simplified.RData")
