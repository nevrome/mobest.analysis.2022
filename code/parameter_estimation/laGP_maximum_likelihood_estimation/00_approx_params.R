library(magrittr)
library(laGP)

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final

#### approximation with mleGPsep ####

independent <- anno %>%
  dplyr::transmute(
    x = x/1000000,
    y = y/1000000,
    z = calage_center/1000
  )

dependent <- anno$PC1

mleGPsep_out <- lapply(1:10, function(i) {
  da <- laGP::darg(list(mle = TRUE), independent)
  gp <- laGP::newGPsep(
    X = independent, 
    Z = dependent, 
    d = da$start,
    g = 0.01,
    dK = TRUE
  )
  param_estimation <- laGP::mleGPsep(
    gpsepi = gp,
    param = "d",
    tmin = da$min, tmax = da$max, ab = da$ab,
    maxit = 200
  )
  laGP::deleteGPsep(gp)
  return(param_estimation)
})

d <- lapply(mleGPsep_out, function(x) { x$d }) %>% do.call(rbind, .) %>% 
  tidyr::as_tibble() %>% 
  magrittr::set_colnames(c("dx", "dy", "dt")) %>%
  dplyr::mutate(
    dx = dx * 1000,
    dy = dy * 1000,
    dt = dt * 1000
  ) %>%
  dplyr::summarise(
    mean_dx = mean(dx),
    mean_dy = mean(dy),
    mean_dt = mean(dt),
    sd2_dx = 2 * sd(dx),
    sd2_dy = 2 * sd(dy),
    sd2_dt = 2 * sd(dt)
  )

its <- sapply(mleGPsep_out, function(x) { x$it })
mean(its)
2*sd(its)

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
