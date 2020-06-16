library(magrittr)
library(laGP)

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final

#### approximation with mleGPsep ####

# data prep
independent <- anno %>%
  dplyr::transmute(
    x = x/1000000,
    y = y/1000000,
    z = calage_center/1000
  )
dependent <- anno$PC1

# parameter estimation
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

# look at result parameters
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

#### approximation with mleGP ####

scaling_factor_sequence <- c(seq(0.1, 0.9, 0.1), 1, seq(2, 10, 1))

# parameter estimation
mleGP_out <- lapply(scaling_factor_sequence, function(scaling_factor) {
  # data prep inside of parameter estimation, because different scaling factors are tested
  independent <- anno %>%
    dplyr::transmute(
      x = x/1000000,
      y = y/1000000,
      z = calage_center/1000 * scaling_factor
    )
  dependent <- anno$PC1
  da <- laGP::darg(list(mle = TRUE), independent)
  gp <- laGP::newGP(
    X = independent, 
    Z = dependent, 
    d = da$start,
    g = 0.01,
    dK = TRUE
  )
  param_estimation <- laGP::mleGP(
    gpi = gp,
    param = "d",
    tmin = da$min, tmax = da$max, ab = da$ab
  )
  laGP::deleteGP(gp)
  return(param_estimation)
})

d <- sapply(mleGP_out, function(x) { x$d }) * 1000
its <- sapply(mleGP_out, function(x) { x$it })

library(ggplot2)
tibble::tibble(
  scaling_factor = scaling_factor_sequence,
  scaling_factor_fractional = fractional::fractional(scaling_factor_sequence),
  scaling_factor_label = factor(
    as.character(as.character(scaling_factor_fractional)), 
    levels = as.character(as.character(scaling_factor_fractional))
  ),
  d = d
) %>% ggplot() +
  geom_point(
    aes(scaling_factor_label, d)
  )
