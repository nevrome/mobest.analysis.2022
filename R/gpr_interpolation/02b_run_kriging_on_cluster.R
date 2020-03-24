#### run kriging ####

prediction <- lapply(1:nrow(model_grid), function(i) {
  mobest::interpolate_laGP(
    model_grid[["independent_table"]][[i]], 
    model_grid[["dependent_var"]][[i]], 
    pred_grid,
    model_grid[["kernel_setting"]][[i]][["auto"]], 
    model_grid[["kernel_setting"]][[i]][["d"]], 
    model_grid[["kernel_setting"]][[i]][["g"]]
  )
})

save(prediction, file = "/projects1/coest_mobility/coest.interpol.2020/data/gpr/prediction_temporal_sampling.RData")
