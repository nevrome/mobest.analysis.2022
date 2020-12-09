# ~/singularity/slurm_nevrome_coest.sh medium 16 50 code/parameter_estimation/laGP_maximum_likelihood_estimation/anisotropic_mle.R 

library(magrittr)
library(laGP)

#### data ####

load("data/poseidon_data/janno_final.RData")

iterations <- 50

#### approximation with mleGPsep (anisotropic) ####

mleGPsep_out <- lapply(c("C1", "C2"), function(ancestry_component) {
  
  # code fragment if not every every ancestry component is known for each observation
  # .ancestry_component <- rlang::ensym(ancestry_component)
  # janno_final_filtered <- janno_final %>% 
  #   dplyr::select(x, y, Date_BC_AD_Median_Derived, !!.ancestry_component) %>% 
  #   dplyr::filter(!is.na(!!.ancestry_component))
  
  independent <- janno_final %>%
    dplyr::transmute(
      x = x/1000000,
      y = y/1000000,
      z = Date_BC_AD_Median_Derived/1000
    )
  
  dependent <- janno_final[[ancestry_component]]
  
  # parameter estimation
  mleGPsep_params <- lapply(1:iterations, function(i) {
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
      dx = sqrt(x$theta[1]) * 1000, 
      dy = sqrt(x$theta[2]) * 1000, 
      dt = sqrt(x$theta[3]) * 1000, 
      g = x$theta[4], 
      its = x$its,
      msg = x$msg,
      conv = x$conv
    )
  }) %>% dplyr::bind_rows()
  
}) %>% dplyr::bind_rows()


#### approximation with jmleGPsep (anisotropic) ####

jmleGPsep_out <- lapply(c("C1", "C2"), function(ancestry_component) {
  
  independent <- janno_final %>%
    dplyr::transmute(
      x = x/1000000,
      y = y/1000000,
      z = Date_BC_AD_Median_Derived/1000
    )
  
  dependent <- janno_final[[ancestry_component]]
  
  # parameter estimation
  jmleGPsep_params <- lapply(1:iterations, function(i) {
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
      dx = sqrt(d.1) * 1000, 
      dy = sqrt(d.2) * 1000, 
      dt = sqrt(d.3) * 1000, 
      g = g, 
      its = tot.its,
      msg = NA,
      conv = dconv
    ) %>%
    tibble::as_tibble()
  
}) %>% dplyr::bind_rows()

mlesep_out <- rbind(mleGPsep_out, jmleGPsep_out) %>%
  tidyr::pivot_longer(cols = c("dx", "dy", "dt", "g"), names_to = "parameter", values_to = "value") %>% 
  dplyr::mutate(
    parameter = factor(parameter, levels = c("dx", "dy", "dt", "g")),
    mle_method = factor(mle_method, levels = c("mleGPsep", "jmleGPsep")),
    ancestry_component = factor(ancestry_component, levels = c("C1", "C2"))
  )

save(mlesep_out, file = "data/parameter_exploration/mle/mlesep_out.RData")

# scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/mobest.analysis.2020/data/parameter_exploration/mle/mlesep_out.RData data/parameter_exploration/mle/mlesep_out.RData
