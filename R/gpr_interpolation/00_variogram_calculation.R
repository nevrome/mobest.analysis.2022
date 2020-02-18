# https://www.r-bloggers.com/spatio-temporal-kriging-in-r/

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered

#### fit spatiotemporal covariance function ####
anno_ordered <- anno %>% dplyr::arrange(calage_center)

space <- anno %>% dplyr::select(x, y)
sp::coordinates(space) <- ~x+y
sp::proj4string(space) <- sp::CRS("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
#space_df <- sp::SpatialPointsDataFrame(space, data.frame(ID = anno$PC1))

stfdf <- spacetime::STIDF(
  sp = space,
  time = lubridate::as_date("1970-01-01", "Y-M-D") + lubridate::dseconds(anno_ordered$calage_center),
  data = data.frame(PC1 = anno$PC1)
)

var <- gstat::variogramST(
  PC1~1, data = stfdf,
  tunit = "secs",
  assumeRegular = F,
  na.omit = T
)

plot(var)
plot(var, map=F)
plot(var, wireframe=T) 
# 
# gstat::variogram(PC1~1, stfdf)
# metric <- vgmST(
#   "metric",
#   joint = vgm(psill = 1,"Exp", range=5e3, nugget = 1e1),
#   stAni = 1
# )
# metric_Vgm <- fit.StVariogram(var, metric, method="L-BFGS-B",lower=pars.l,tunit="mins")