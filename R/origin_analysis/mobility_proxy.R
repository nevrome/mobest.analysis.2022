library(magrittr)

load("data/pri_ready.RData")
load("data/spatial/mobility_regions.RData")

pri_mean <- mobest::estimate_mobility(pri_ready, mobility_regions)

save(pri_mean, file = "data/mobility_estimation/pri_mean.RData")
