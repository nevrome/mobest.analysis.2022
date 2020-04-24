library(magrittr)

# load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
# anno <- anno_1240K_and_anno_1240K_HumanOrigins_final %>%
#   dplyr::filter(
#     region_id == "Central Europe",
#     calage_center > -4000, calage_center < -2300
#   )
# 
# x1 <- anno$x
# x2 <- anno$y
# x3 <- anno$calage_center
# y <- anno$PC1

independent <- tibble::tibble(
  x1 <- seq(10, 200, 10),
  x2 <- 21:40,
  x3 <- 41:60
)
dependent <- x1 + x2 + x3 + rnorm(length(x1), 0, 5)

# linear fit
combined <- independent %>% dplyr::mutate(d = dependent)
model <- stats::lm(d ~ x1 + x2 + x3, data = combined)
dependent <- model[["residuals"]]


fit <- rstan::stan(
  file = "code/bayesian_inference/gpr.stan", 
  data = list(
    D = 3,
    x = data.frame(x1, x2, x3),
    N = length(dependent),
    y = dependent
  ),
  chains = 1,
  cores = 5
)

rstan::plot(fit)

ex <- rstan::extract(fit)

ex$theta[,1] %>% hist()
ex$theta[,2] %>% hist()
ex$theta[,3] %>% hist()

ex$nugget %>% hist()

