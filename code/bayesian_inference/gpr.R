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

# world <- expand.grid(
#   x1 = 1:3,
#   x2 = 1:3,
#   x3 = c(1, 10, 100)
# ) %>%
#   dplyr::mutate(
#     y = x1 + x2
#   ) %>%
#   dplyr::group_by(x1, x2) %>%
#   dplyr::mutate(
#     y = y / x3
#   ) %>% 
#   dplyr::ungroup()
# 
# # linear fit
# # model <- stats::lm(y ~ x1 + x2 + x3, data = world)
# 
# ind <- world %>% dplyr::select(x1, x2, x3)
# # dep <- model[["residuals"]]
# dep <- world$y

world <- expand.grid(
  x1 = 1:3,
  x2 = 1:3,
  x3 = 1:3
)
world$y <- c(rep(-100, 9), rep(0, 9), rep(100, 9))

ind <- world %>% dplyr::select(x1, x2, x3)
dep <- world$y

fit <- rstan::stan(
  file = "code/bayesian_inference/gpr.stan", 
  data = list(
    D = 3,
    x = ind,
    N = length(dep),
    y = dep
  ),
  chains = 1,
  cores = 1
)

rstan::plot(fit)

ex <- rstan::extract(fit)

ex$theta[,1] %>% hist()
ex$theta[,2] %>% hist()
ex$theta[,3] %>% hist()

ex$nugget %>% hist()

