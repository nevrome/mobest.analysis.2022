# https://www.r-bloggers.com/spatio-temporal-kriging-in-r/
library(magrittr)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered

#### fit spatiotemporal covariance function ####
space <- anno %>% dplyr::select(x, y)
sp::coordinates(space) <- ~x+y
sp::proj4string(space) <- sp::CRS("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
#space_df <- sp::SpatialPointsDataFrame(space, data.frame(ID = anno$PC1))

stfdf <- spacetime::STIDF(
  sp = space,
  time = lubridate::as_date("0000-01-01", "Y-M-D") + lubridate::dseconds(anno$calage_center * (60/100)),
  data = data.frame(PC1 = anno$PC1)
)

spacetime::stplot(stfdf)

var <- gstat::variogramST(
  PC1~1, data = stfdf,
  tunit = "mins",
  tlags = 1:80,
  boundaries = seq(100000, 5100000, 100000),
  #cutoff = 1000000,
  assumeRegular = F,
  na.omit = T
)

plot(var, map = F)
plot(var)
plot(var, wireframe = T) 

save(var, file = "data/gstats_spacetime_variogram.RData")

# pars.l <- c(sill.s = 0, range.s = 10, nugget.s = 0,sill.t = 0, range.t = 1, nugget.t = 0,sill.st = 0, range.st = 10, nugget.st = 0, anis = 0)
# finalVgmMSE <- Inf
# finalVgm <- NULL
# for( anisotropy in seq(10000, 100000, 10000)){
#   try( {
#     metric <- gstat::vgmST("metric", joint = gstat::vgm(psill = 0.02, model = "Exp", range = 1000000), stAni = anisotropy)
#     metric_Vgm <- gstat::fit.StVariogram(var, metric, method = "L-BFGS-B", lower = pars.l)
#     mse <- attr(metric_Vgm,"MSE")
#     print(paste0("Anisotropy: ", anisotropy, "; MSE: ", mse))
#     if(mse < finalVgmMSE){
#       finalVgmMSE <- mse
#       finalVgm <- metric_Vgm
#     }
#   }, silent = FALSE)
# }
# 
# finalVgm
#  
# metric1 <- gstat::vgmST("metric", joint = gstat::vgm(psill = 0.01, model = "Exp", range = 1000000, nugget = 0.01), stAni = 50000)
# metric2 <- gstat::vgmST("metric", joint = gstat::vgm(psill = 0.01, model = "Exp", range = 1000000, nugget = 0), stAni = 50000)
# 
# metric_Vgm1 <- gstat::fit.StVariogram(var, metric1, method = "L-BFGS-B", lower = pars.l)
# metric_Vgm2 <- gstat::fit.StVariogram(var, metric2, method = "L-BFGS-B", lower = pars.l)
# 
# attr(metric_Vgm1, "MSE")
# attr(metric_Vgm2, "MSE")
# 
# plot(var, metric_Vgm1, map = F) 
# plot(var, metric_Vgm2, map = F) 
# 
# attributes(metric_Vgm1)

# plot(
#   var,
#   list(separable_Vgm, prodSumModel_Vgm, metric_Vgm, sumMetric_Vgm, SimplesumMetric_Vgm),
#   all=T,
#   wireframe=T
# )
