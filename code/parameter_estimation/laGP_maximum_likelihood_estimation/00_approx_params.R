library(magrittr)
library(laGP)

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final

#### approximation with mleGPsep ####

mleGPsep_out <- lapply(c("PC1", "PC2", "C1", "C2"), function(ancestry_component) {
  
  .ancestry_component <- rlang::ensym(ancestry_component)
  
  anno_filtered <- anno %>% dplyr::select(x, y, calage_center, !!.ancestry_component) %>% 
    dplyr::filter(!is.na(!!.ancestry_component))
  
  independent <- anno_filtered %>%
    dplyr::transmute(
      x = x/1000000,
      y = y/1000000,
      z = calage_center/1000
    )
  
  dependent <- anno_filtered[[ancestry_component]]
  
  # parameter estimation
  mleGPsep_params <- lapply(1:100, function(i) {
    da <- laGP::darg(list(mle = TRUE), independent)
    ga <- laGP::garg(list(mle = TRUE), dependent)
    gp <- laGP::newGPsep(
      X = independent, 
      Z = dependent, 
      d = da$start,
      g = ga$start,
      dK = TRUE
    )
    param_estimation <- laGP::mleGPsep(
      gpsepi = gp,
      param = "both",
      tmin = c(da$min, ga$min), tmax = c(da$max, ga$max), ab = c(da$ab, ga$ab),
      maxit = 200
    )
    laGP::deleteGPsep(gp)
    return(param_estimation)
  })
  
  # look at result parameters
  lapply(mleGPsep_params, function(x) { 
    tibble::tibble(
      mle_method = "mleGPsep",
      ancestry_component = ancestry_component,
      dx = x$theta[1] * 1000, 
      dy = x$theta[2] * 1000, 
      dt = x$theta[3] * 1000, 
      g = x$theta[4], 
      its = x$its,
      msg = x$msg,
      conv = x$conv
    )
  }) %>% dplyr::bind_rows()
  
}) %>% dplyr::bind_rows()


#### approximation with jmleGPsep ####

jmleGPsep_out <- lapply(c("PC1", "PC2", "C1", "C2"), function(ancestry_component) {
  
  .ancestry_component <- rlang::ensym(ancestry_component)
  
  anno_filtered <- anno %>% dplyr::select(x, y, calage_center, !!.ancestry_component) %>% 
    dplyr::filter(!is.na(!!.ancestry_component))
  
  independent <- anno_filtered %>%
    dplyr::transmute(
      x = x/1000000,
      y = y/1000000,
      z = calage_center/1000
    )
  
  dependent <- anno_filtered[[ancestry_component]]
  
  # parameter estimation
  jmleGPsep_params <- lapply(1:100, function(i) {
    da <- laGP::darg(list(mle = TRUE), independent)
    ga <- laGP::garg(list(mle = TRUE), dependent)
    gp <- laGP::newGPsep(
      X = independent, 
      Z = dependent, 
      d = da$start,
      g = ga$start,
      dK = TRUE
    )
    param_estimation <- laGP::jmleGPsep(
      gpsepi = gp,
      drange = c(da$min, da$max),
      grange = c(ga$min, ga$max),
      dab = da$ab, 
      gab = ga$ab,
      maxit = 200
    )
    laGP::deleteGPsep(gp)
    return(param_estimation)
  })
  
  # look at result parameters
  jmleGPsep_params %>% 
    dplyr::bind_rows() %>%
    dplyr::transmute(
      mle_method = "jmleGPsep",
      ancestry_component = ancestry_component,
      dx = d.1 * 1000, 
      dy = d.2 * 1000, 
      dt = d.3 * 1000, 
      g = g, 
      its = tot.its,
      msg = NA,
      conv = dconv
    ) %>%
    tibble::as_tibble()
  
}) %>% dplyr::bind_rows()

mle_out <- rbind(mleGPsep_out, jmleGPsep_out) %>%
  tidyr::pivot_longer(cols = c("dx", "dy", "dt", "g"), names_to = "parameter", values_to = "value") %>% 
  dplyr::mutate(
    parameter = factor(parameter, levels = c("dx", "dy", "dt", "g")),
    mle_method = factor(mle_method, levels = c("mleGPsep", "jmleGPsep")),
    ancestry_component = factor(ancestry_component, levels = c("PC1", "PC2", "C1", "C2"))
  )

save(mle_out, file = "data/parameter_exploration/mle_out.RData")










# 
# 
# #### approximation with mleGP ####
# 
# scaling_factor_sequence <- c(seq(0.1, 0.9, 0.1), 1, seq(2, 10, 1))
# 
# # parameter estimation
# mleGP_out <- lapply(scaling_factor_sequence, function(scaling_factor) {
#   # data prep inside of parameter estimation, because different scaling factors are tested
#   independent <- anno %>%
#     dplyr::transmute(
#       x = x/1000000,
#       y = y/1000000,
#       z = calage_center/1000 * scaling_factor
#     )
#   dependent <- anno$PC1
#   da <- laGP::darg(list(mle = TRUE), independent)
#   gp <- laGP::newGP(
#     X = independent, 
#     Z = dependent, 
#     d = da$start,
#     g = 0.01,
#     dK = TRUE
#   )
#   param_estimation <- laGP::mleGP(
#     gpi = gp,
#     param = "d",
#     tmin = da$min, tmax = da$max, ab = da$ab
#   )
#   laGP::deleteGP(gp)
#   return(param_estimation)
# })
# 
# d <- sapply(mleGP_out, function(x) { x$d }) * 1000
# its <- sapply(mleGP_out, function(x) { x$it })
# 
# library(ggplot2)
# tibble::tibble(
#   scaling_factor = scaling_factor_sequence,
#   scaling_factor_fractional = fractional::fractional(scaling_factor_sequence),
#   scaling_factor_label = factor(
#     as.character(as.character(scaling_factor_fractional)), 
#     levels = as.character(as.character(scaling_factor_fractional))
#   ),
#   d = d
# ) %>% ggplot() +
#   geom_point(
#     aes(scaling_factor_label, d)
#   )
