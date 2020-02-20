library(magrittr)
library(laGP)
source("R/helper_functions.R")

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered

#### prep independent variables ####

independent <- anno %>%
  dplyr::transmute(
    x_01 = range_01_x(x),
    y_01 = range_01_y(y),
    z_01 = range_01_z(calage_center)
  )

dependent <- anno$PC1
r <- 1:nrow(anno)
index_training <- sample(r, 0.9 * length(r))
index_test <- r[-index_training]

func <- function(par) {
  gp <- newGPsep(
    X = independent[index_training,], 
    Z = dependent[index_training], 
    d = c(dist_scale_01_x_km(par[1]), dist_scale_01_y_km(par[1]), dist_scale_01_z_y(par[2])), 
    g = 0.0001, #par[3]/1000,
    dK = TRUE
  )
  pred <- predGPsep(gp, XX = independent[index_test,], lite = T)
  deleteGPsep(gp)
  res <- sum(abs(dependent[index_test] - pred$mean))
  #message(paste("space =", par[1], "time =", par[2], "g =", par[3], "res =", res))
  return(res)
}

#optim(par = c(100, 100, 100), func, lower = c(10, 10, 10), upper = c(1000, 1000, 1000), method = "L-BFGS-B")
optim(par = c(100, 100), func, lower = c(10, 10), upper = c(1000, 1000), method = "L-BFGS-B")

