library(magrittr)
library(ggplot2)

load("data/genotype_data/janno_final.RData")

d_all <- mobest::calculate_pairwise_distances(
  independent = mobest::create_spatpos(
    id = janno_final$Poseidon_ID,
    x = janno_final$x,
    y = janno_final$y,
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = mobest::create_obs(
    C1 = janno_final$C1,
    C2 = janno_final$C2,
    C3 = janno_final$C3
  )
)

save(d_all, file = "data/parameter_exploration/variogram/all_distances.RData")

d_binned <- mobest::bin_pairwise_distances(d_all)

save(d_binned, file = "data/parameter_exploration/variogram/binned_distances.RData")
