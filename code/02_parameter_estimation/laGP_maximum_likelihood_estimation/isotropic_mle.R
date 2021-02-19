library(magrittr)
library(laGP)

load("data/poseidon_data/janno_final.RData")

mle_out <- mobest::laGP_mle_sequence_isotropic_fixed_g(
  independent = mobest::create_spatpos(
    id = janno_final$Individual_ID,
    x = janno_final$x / 1000,
    y = janno_final$y / 1000,
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = mobest::create_obs(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  iterations = 2,
  g = 0.08,
  space_time_scaling_factor_sequence = c(seq(0.1, 0.9, 0.1), 1, seq(2, 10, 1))
)

save(mle_out, file = "data/parameter_exploration/mle/mle_out.RData")



