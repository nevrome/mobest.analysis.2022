# ./sge_parameter_exploration.sh

library(magrittr)

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
run <- args[1]
ds_for_this_run <- as.numeric(args[2])
dt_for_this_run <- as.numeric(args[3])
g_for_this_run <- as.numeric(args[4])

#### data ####

load("data/poseidon_data/janno_final.RData")

interpol_comparison <- mobest::crossvalidate(
  independent = mobest::create_spatpos(
    id = 1:nrow(janno_final),
    x = janno_final$x, 
    y = janno_final$y, 
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = mobest::create_obs(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  kernel = mobest::create_kernset_cross(
    ds = ds_for_this_run*1000,
    dt = dt_for_this_run, 
    g = g_for_this_run
  ),
  iterations = 2,
  groups = 10,
  quiet = F
)

interpol_comparison$ds <- interpol_comparison$ds/1000

save(interpol_comparison, file = paste0(
  "data/parameter_exploration/crossvalidation/interpol_comparison_", 
  run, 
  ".RData"
))
