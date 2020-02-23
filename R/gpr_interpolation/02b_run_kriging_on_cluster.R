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
  
#library(magrittr)
library(laGP)

#### kriging function ####

predictgp <- function(independent, dependent, pred_grid, auto = T, d, g) {
  # priors for the global GP
  if (auto) {
    da <- darg(list(mle = TRUE, max=10), independent)
    ga <- garg(list(mle = TRUE, max=10), dependent)
    d <- da$start
    g <- ga$start
  }
  # fit the global GP
  gp <- newGPsep(X = independent, Z = dependent, d = d, g = g, dK = auto)
  # optimise fit automatically
  if (auto) {
    mleGPsep(
      gpsepi = gp, 
      param = "both", 
      tmin = c(da$min, ga$min), tmax = c(da$max, ga$max), ab = c(da$ab, ga$ab), 
      maxit = 200
    )
  }
  # predictions from the global GP on the prediction
  pred <- predGPsep(gp, XX = pred_grid[, c("x_01", "y_01", "z_01")], lite = T)
  # delete GP object
  deleteGPsep(gp)
  # return result 
  return(pred)
}

#### run kriging ####

model_grid$prediction <- lapply(1:nrow(model_grid), function(i) {
  predictgp(
    model_grid[["independent_table"]][[i]], 
    model_grid[["dependent_var"]][[i]], 
    pred_grid,
    model_grid[["kernel_setting"]][[i]][["auto"]], 
    model_grid[["kernel_setting"]][[i]][["d"]], 
    model_grid[["kernel_setting"]][[i]][["g"]]
  )
})

save(model_grid, file = "/projects1/coest_mobility/coest.interpol.2020/data/gpr/model_grid_temporal_sampling.RData")
