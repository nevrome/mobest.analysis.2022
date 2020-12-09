library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")

d_all <- mobest::calculate_pairwise_distances(
  mobest::create_spatpos(
    id = janno_final$Individual_ID,
    x = janno_final$x,
    y = janno_final$y,
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  mobest::create_obs(
    id = janno_final$Individual_ID,
    C1 = janno_final$C1,
    C2 = janno_final$C2
  )
)

save(d_all, file = "data/parameter_exploration/variogram/all_distances.RData")

