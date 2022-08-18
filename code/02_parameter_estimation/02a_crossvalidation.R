# run all:
# qsub code/02_parameter_estimation/02b_sge_crossvalidation.shq

# run one (in case a script randomly fails when running all): 
# qsub -b y -cwd -q archgen.q -pe smp 8 -l h_vmem=50G -now n -V -j y -o ~/log -N fillGaps singularity exec --bind=/mnt/archgen/users/schmid singularity_mobest.sif Rscript code/02_parameter_estimation/02a_crossvalidation.R "11772" "pca_proj" "u" "C9" "500" "1300"

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
load("data/parameter_exploration/distance_products.RData")

mperm <- as.data.frame(multivar_method_permutations)
mperm_id <- which(mperm$method == multivar_for_this_run & mperm$fstate == snpset_for_this_run)

nugget_for_this_run <- distance_products %>%
  dplyr::filter(
    dim == dimension_for_this_run,
    multivar_method == multivar_for_this_run,
    multivar_fstate == snpset_for_this_run
  ) %$%
  estimated_nugget

kernel_for_this_run <- mobest::create_kernset_multi(
  kernel_for_this_run = mobest::create_kernset(
    mobest::create_kernel(
      ds_for_this_run*1000,
      ds_for_this_run*1000,
      dt_for_this_run,
      nugget_for_this_run
    ),
    .names = paste(dimension_for_this_run, multivar_for_this_run, snpset_for_this_run, sep = "_")
  )
)

#### run crossvalidation ####

interpol_comparison_raw <- mobest::crossvalidate(
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

interpol_comparison <- interpol_comparison_raw %>%
  dplyr::select(-independent_table_id, -dependent_setting_id, -kernel_setting_id, -pred_grid_id) %>%
  dplyr::mutate(
    dsx = dsx/1000,
    dsy = dsy/1000
  ) %>%
  tibble::add_column(
    dim = dimension_for_this_run,
    multivar_method = multivar_for_this_run,
    multivar_fstate = snpset_for_this_run,
    .after = "dependent_var_id"
  )

save(interpol_comparison, file = paste0(
  "data/parameter_exploration/crossvalidation/interpol_comparison_",
  dimension_for_this_run, "_",
  multivar_for_this_run, "_",
  snpset_for_this_run, "_",
  run, 
  ".RData"
))
