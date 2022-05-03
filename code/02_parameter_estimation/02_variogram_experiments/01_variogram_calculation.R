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
    C1_mds_u = janno_final$C1_mds_u,
    C2_mds_u = janno_final$C2_mds_u,
    C3_mds_u = janno_final$C3_mds_u
  )
)

save(d_all, file = "data/parameter_exploration/variogram/all_distances.RData")

d_binned <- mobest::bin_pairwise_distances(d_all, geo_bin = 100, time_bin = 100)

save(d_binned, file = "data/parameter_exploration/variogram/binned_distances.RData")
