library(magrittr)
library(laGP)

#### data ####

load("data/poseidon_data/janno_final.RData")

#### approximation with mleGP (isotropic) ####
scaling_factor_sequence <- c(seq(0.1, 0.9, 0.1), 1, seq(2, 10, 1))
ancestry_components <- c("C1", "C2")

# parameter estimation
mle_out <- lapply(ancestry_components, function(ancestry_component) {
  
  mleGP_out_list <- lapply(scaling_factor_sequence, function(scaling_factor) {
    # data prep inside of parameter estimation, because different scaling factors are tested
    independent <- janno_final %>%
      dplyr::transmute(
        x = x/1000000,
        y = y/1000000,
        z = Date_BC_AD_Median_Derived/1000 * scaling_factor
      )
    dependent <- janno_final[[ancestry_component]]
    # run laGP parameter estimation
    da <- laGP::darg(list(mle = TRUE), independent)
    gp <- laGP::newGP(
      X = independent,
      Z = dependent,
      d = da$start,
      g = 0.04,
      dK = TRUE
    )
    param_estimation <- laGP::mleGP(
      gpi = gp,
      param = "d",
      tmin = da$min, tmax = da$max, ab = da$ab,
      verb = 1
    )
    param_estimation$l <- laGP::llikGP(gp, dab = da$ab)
    laGP::deleteGP(gp)
    return(param_estimation)
  })
  
  # combine list to better readable data.frame
  mle_out <- tibble::tibble(
    ancestry_component = ancestry_component,
    scaling_factor = scaling_factor_sequence,
    scaling_factor_fractional = fractional::fractional(scaling_factor_sequence),
    scaling_factor_label = factor(
      as.character(as.character(scaling_factor_fractional)),
      levels = as.character(as.character(scaling_factor_fractional))
    ),
    # rescaling
    d = sqrt(sapply(mleGP_out_list, function(x) { x$d })) * 1000,
    l = sapply(mleGP_out_list, function(x) { x$l }),
    its = sapply(mleGP_out_list, function(x) { x$it })
  ) %>% dplyr::mutate(
    ds = d,
    dt = d / scaling_factor
  )
  
  return(mle_out)
}) %>% dplyr::bind_rows()

save(mle_out, file = "data/parameter_exploration/mle/mle_out.RData")
