# ~/singularity/slurm_nevrome_coest.sh medium 16 50 code/parameter_estimation/laGP_maximum_likelihood_estimation/anisotropic_mle.R 

library(magrittr)
library(laGP)

#### data ####

load("data/poseidon_data/janno_final.RData")

#### approximation with mleGPsep (anisotropic) ####

mleGPsep_out <- mobest::laGP_mle_anisotropic(
  independent = mobest::create_spatpos(
    id = janno_final$Individual_ID,
    x = janno_final$x / 1000,
    y = janno_final$y / 1000,
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = list(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  iterations = 50
)

#### approximation with jmleGPsep (anisotropic) ####

jmleGPsep_out <- mobest::laGP_jmle_anisotropic(
  independent = mobest::create_spatpos(
    id = janno_final$Individual_ID,
    x = janno_final$x / 1000,
    y = janno_final$y / 1000,
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = list(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  iterations = 50
)

#### merge result ####

mlesep_out <- rbind(mleGPsep_out, jmleGPsep_out) %>%
  tidyr::pivot_longer(cols = c("dx", "dy", "dt", "g"), names_to = "parameter", values_to = "value") %>% 
  dplyr::mutate(
    parameter = factor(parameter, levels = c("dx", "dy", "dt", "g")),
    mle_method = factor(mle_method, levels = c("mleGPsep", "jmleGPsep")),
    ancestry_component = factor(ancestry_component, levels = c("C1", "C2"))
  )

save(mlesep_out, file = "data/parameter_exploration/mle/mlesep_out.RData")

# scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/mobest.analysis.2020/data/parameter_exploration/mle/mlesep_out.RData data/parameter_exploration/mle/mlesep_out.RData
