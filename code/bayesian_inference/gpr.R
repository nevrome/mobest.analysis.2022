library(magrittr)

world <- expand.grid(
  x1 = seq(10, 30, 10),
  x2 = seq(10, 30, 10),
  x3 = seq(10, 30, 10)
) %>%
  dplyr::mutate(
    x1 = x1 + rnorm(nrow(.), 0, 2),
    x2 = x2 + rnorm(nrow(.), 0, 2),
    x3 = x3 + rnorm(nrow(.), 0, 2)
  )
world$y <- c(rep(-100, 9), rep(0, 9), rep(100, 9)) + rnorm(nrow(world), 0, 5)

ind <- world %>% dplyr::select(x1, x2, x3)
dep <- world$y


fit <- rstan::stan(
  file = "code/bayesian_inference/gpr.stan",
  data = list(
    x = ind,
    N = length(dep),
    y = dep
  ),
  chains = 1,
  cores = 1,
  control = list(max_treedepth = 10)
)


rstan::plot(fit)

ex <- rstan::extract(fit)

ex$theta[,1] %>% hist()
ex$theta[,2] %>% hist()
ex$theta[,3] %>% hist()

ex$nugget %>% hist()

####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")

anno <- anno_1240K_and_anno_1240K_HumanOrigins_final %>%
  dplyr::filter(
    region_id == "Central Europe",
    calage_center > -4000, calage_center < -2300
  )
# ds = 97.65 +- 6.78
# dt = 2700.05 +- 169.37
# g = 0.00 +- 0.000
# sigma = 0.00 +- 0.000

anno <- anno_1240K_and_anno_1240K_HumanOrigins_final %>%
  dplyr::filter(
    region_id == "Central Europe",
    calage_center > -3000, calage_center < -1000
  )
# ds = 2534.68 +- 39.58
# dt = 282.67 +- 5.49
# g = 0.00 +- 0.000
# sigma = 0.00 +- 0.000

fit <- rstan::stan(
  file = "code/bayesian_inference/gpr.stan",
  data = list(
    x = data.frame(x1 = anno$x/1000, x2 = anno$y/1000, x3 = anno$calage_center),
    N = length(anno$PC1),
    y = anno$PC1
  ),
  chains = 1,
  cores = 1,
  warmup = 300,
  iter = 400,
  control = list(max_treedepth = 10)
)




