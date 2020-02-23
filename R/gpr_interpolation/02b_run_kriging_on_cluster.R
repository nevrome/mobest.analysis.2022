v <- floor(as.numeric(R.Version()$minor))

if (v == 4) {
  .libPaths( c( "/projects1/clusterhomes/schmid/R/x86_64-pc-linux-gnu-library/3.4" , .libPaths() ) )
  load("/projects1/coest_mobility/coest.interpol.2020/data/gpr/gpr_prep_temporal_sampling_v2.RData")
} else if ( v == 5) {
  .libPaths( c( "/projects1/clusterhomes/schmid/R/x86_64-pc-linux-gnu-library/3.5" , .libPaths() ) )
  load("/projects1/coest_mobility/coest.interpol.2020/data/gpr/gpr_prep_temporal_sampling_v3.RData")
} else if ( v == 6) {
  .libPaths( c( "/projects1/clusterhomes/schmid/R/x86_64-pc-linux-gnu-library/3.6" , .libPaths() ) )
  load("/projects1/coest_mobility/coest.interpol.2020/data/gpr/gpr_prep_temporal_sampling_v3.RData")
}
  
#library(magrittr)
library(laGP)

#### kriging function ####

predictgp <- function(independent, dependent, pred_grid) {
  # priors for the global GP
  da <- darg(list(mle = TRUE, max=10), independent)
  ga <- garg(list(mle = TRUE, max=10), dependent)
  # fit the global GP
  gp <- newGPsep(
    X = independent, Z = dependent, 
    d = da$start, g = ga$start,
    dK = TRUE
  )
  mleGPsep(
    gpsepi = gp, 
    param = "both", 
    tmin = c(da$min, ga$min), tmax = c(da$max, ga$max), ab = c(da$ab, ga$ab), 
    maxit = 200
  )
  # predictions from the global GP on the prediction
  pred <- predGPsep(gp, XX = pred_grid[, c("x_01", "y_01", "z_01")], lite = T)
  # delete GP object
  deleteGPsep(gp)
  # return result 
  return(pred)
}

#### run kriging ####
# for every PC
prediction_list <- lapply(c("PC1", "PC2", "PC3", "PC4"), function(x, anno, pred_grid) {
  # for every time sampling run
  lapply(independent_list, function(y, x) {
      predictgp(y, anno[[x]], pred_grid)
  }, x)
}, anno, pred_grid)

save(prediction_list, file = "/projects1/coest_mobility/coest.interpol.2020/data/gpr/prediction_list_temporal_sampling.RData")
