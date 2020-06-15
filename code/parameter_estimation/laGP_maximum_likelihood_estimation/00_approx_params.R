library(magrittr)
library(laGP)

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final

#### prep independent variables ####

independent <- anno %>%
  dplyr::transmute(
    x = x/1000000,
    y = y/1000000,
    z = calage_center/1000
  )

dependent <- anno$PC1

#### approximation with mleGP ####

da <- laGP::darg(list(mle = TRUE), independent)

gp <- laGP::newGPsep(
  X = independent, 
  Z = dependent, 
  d = da$start,
  g = 0.01,
  dK = TRUE
)

laGP::mleGPsep(
  gpsepi = gp,
  param = "d",
  tmin = da$min, tmax = da$max,
  maxit = 200
)

laGP::deleteGPsep(gp)

# gp

gp <- laGP::newGP(
  X = independent, 
  Z = dependent, 
  d = da$start,
  g = 0.01,
  dK = TRUE
)

res <- laGP::mleGP(
  gpi = gp,
  param = "d",
  tmin = da$min, tmax = da$max
)

res$d * 1000
