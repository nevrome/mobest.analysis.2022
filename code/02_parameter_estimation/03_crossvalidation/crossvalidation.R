# qsub code/02_parameter_estimation/crossvalidation/sge_parameter_exploration.sh

library(magrittr)

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
run <- args[1]
ds_for_this_run <- as.numeric(args[2])
dt_for_this_run <- as.numeric(args[3])

#### data ####

load("data/genotype_data/janno_final.RData")
load("data/parameter_exploration/variogram/estimated_nuggets.RData")

interpol_comparison <- mobest::crossvalidate(
  independent = mobest::create_spatpos(
    id = 1:nrow(janno_final),
    x = janno_final$x, 
    y = janno_final$y, 
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = mobest::create_obs(
    C1_mds_u = janno_final$C1_mds_u,
    C2_mds_u = janno_final$C2_mds_u,
    C3_mds_u = janno_final$C3_mds_u
  ),
  kernel = mobest::create_kernset_multi(
    mobest::create_kernset(
      C1_mds_u = mobest::create_kernel(
        ds_for_this_run*1000, ds_for_this_run*1000, dt_for_this_run, 
        estimated_nuggets$nugget[estimated_nuggets$dependent_var_id == "C1_mds_u"]
      ),
      C2_mds_u = mobest::create_kernel(
        ds_for_this_run*1000, ds_for_this_run*1000, dt_for_this_run,
        estimated_nuggets$nugget[estimated_nuggets$dependent_var_id == "C2_mds_u"]
      ),
      C3_mds_u = mobest::create_kernel(
        ds_for_this_run*1000, ds_for_this_run*1000, dt_for_this_run, 
        estimated_nuggets$nugget[estimated_nuggets$dependent_var_id == "C3_mds_u"]
      )
    ),
    .names = paste0("kernel_", run)
  ),
  iterations = 10,
  groups = 10,
  quiet = F
)

interpol_comparison$dsx <- interpol_comparison$dsx/1000
interpol_comparison$dsy <- interpol_comparison$dsy/1000

save(interpol_comparison, file = paste0(
  "data/parameter_exploration/crossvalidation/interpol_comparison_", 
  run, 
  ".RData"
))
