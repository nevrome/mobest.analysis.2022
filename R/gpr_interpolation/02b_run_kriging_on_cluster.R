#### load correct data and software for this cluster node ####

v <- floor(as.numeric(R.Version()$minor))
if (v == 4) {
  .libPaths( c( "/projects1/clusterhomes/schmid/R/x86_64-pc-linux-gnu-library/3.4" , .libPaths() ) )
  load("/projects1/coest_mobility/coest.interpol.2020/data/gpr/gpr_pred_grid_temporal_sampling_v2.RData")
  load("/projects1/coest_mobility/coest.interpol.2020/data/gpr/gpr_model_grid_temporal_sampling_v2.RData")
} else if ( v == 5) {
  .libPaths( c( "/projects1/clusterhomes/schmid/R/x86_64-pc-linux-gnu-library/3.5" , .libPaths() ) )
  load("/projects1/coest_mobility/coest.interpol.2020/data/gpr/gpr_pred_grid_temporal_sampling_v3.RData")
  load("/projects1/coest_mobility/coest.interpol.2020/data/gpr/gpr_model_grid_temporal_sampling_v3.RData")
} else if ( v == 6) {
  .libPaths( c( "/projects1/clusterhomes/schmid/R/x86_64-pc-linux-gnu-library/3.6" , .libPaths() ) )
  load("/projects1/coest_mobility/coest.interpol.2020/data/gpr/gpr_pred_grid_temporal_sampling_v3.RData")
  load("/projects1/coest_mobility/coest.interpol.2020/data/gpr/gpr_model_grid_temporal_sampling_v3.RData")
}
  
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
