library(magrittr)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final %>%
  dplyr::filter(
    region_id == "Central Europe",
    calage_center > -4000, calage_center < -2300
  )

x1 <- anno$x
x2 <- anno$y
x3 <- anno$calage_center
y <- anno$PC1

fit <- rstan::stan(
  file = "code/bayesian_inference/gpr.stan", 
  data = list(
    D = 3,
    x = array(c(x1, x2, x3), dim = c(length(x1), 3)),
    N = length(y),
    y = y
  ),
  chains = 1,
  cores = 5
)

rstan::plot(fit)

ex <- rstan::extract(fit)


