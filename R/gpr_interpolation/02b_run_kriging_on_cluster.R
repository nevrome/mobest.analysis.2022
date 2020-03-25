load("data/gpr/gpr_pred_grid_temporal_sampling.RData")
load("data/gpr/gpr_model_grid_temporal_sampling.RData")

#### run kriging ####
prediction <- lapply(1:nrow(model_grid), function(i) {
  mobest::interpolate_laGP(
    independent = model_grid[["independent_table"]][[i]], 
    dependent = model_grid[["dependent_var"]][[i]], 
    pred_grid = pred_grid,
    auto = model_grid[["kernel_setting"]][[i]][["auto"]], 
    d = model_grid[["kernel_setting"]][[i]][["d"]], 
    g = model_grid[["kernel_setting"]][[i]][["g"]]
  )
})

save(prediction, file = "data/gpr/prediction_temporal_sampling.RData")
#save(prediction, file = "/projects1/coest_mobility/coest.interpol.2020/data/gpr/prediction_temporal_sampling.RData")
