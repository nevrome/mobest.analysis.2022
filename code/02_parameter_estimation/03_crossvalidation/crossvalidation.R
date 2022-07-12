# qsub code/02_parameter_estimation/03_crossvalidation/sge_parameter_exploration.shq

library(magrittr)

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
run <- args[1]
multivar_for_this_run <- args[2]
snpset_for_this_run <- args[3]
dimension_for_this_run <- args[4]
ds_for_this_run <- as.numeric(args[5])
dt_for_this_run <- as.numeric(args[6])

#### data ####

load("data/genotype_data/janno_final.RData")
load("data/genotype_data/multivar_perm_obs_bundles.RData")
load("data/parameter_exploration/multivariate_analysis_comparison/distance_products.RData")

mperm <- as.data.frame(multivar_method_permutations)
mperm_id <- which(mperm$method == multivar_for_this_run & mperm$fstate == snpset_for_this_run)

nugget_for_this_run <- distance_products %>%
  dplyr::filter(
    dim == dimension_for_this_run,
    method == multivar_for_this_run,
    snp_selection == snpset_for_this_run
  ) %$%
  estimated_nugget

kernel_for_this_run <- mobest::create_kernset_multi(
  kernel_for_this_run = mobest::create_kernset(
    mobest::create_kernel(ds_for_this_run*1000, ds_for_this_run*1000, dt_for_this_run, nugget_for_this_run),
    .names = paste(dimension_for_this_run, multivar_for_this_run, snpset_for_this_run, sep = "_")
  )
)

#### run crossvalidation ####

interpol_comparison <- mobest::crossvalidate(
  independent = mobest::create_spatpos(
    id = 1:nrow(janno_final),
    x = janno_final$x, 
    y = janno_final$y, 
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = mobest::create_obs(
    multivar_method_observation_bundles[[mperm_id]][[dimension_for_this_run]],
    .names = paste(dimension_for_this_run, multivar_for_this_run, snpset_for_this_run, sep = "_")
  ),
  kernel = kernel_for_this_run,
  iterations = 10,
  groups = 10,
  quiet = F
)

interpol_comparison$dsx <- interpol_comparison$dsx/1000
interpol_comparison$dsy <- interpol_comparison$dsy/1000

save(interpol_comparison, file = paste0(
  "data/parameter_exploration/crossvalidation/interpol_comparison_",
  dimension_for_this_run, "_",
  multivar_for_this_run, "_",
  snpset_for_this_run, "_",
  run, 
  ".RData"
))
