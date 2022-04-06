library(magrittr)

#### load command line arguments ####

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
run <- as.integer(args[1])

#### load data ####

load("data/genotype_data/janno_final.RData")
load("data/genotype_data/multivar_perm_obs_bundles.RData")
load("data/parameter_exploration/multivariate_analysis_comparison/distance_products.RData")

#### run crossvalidation ####

multivar_comparison <- mobest::crossvalidate(
  independent = mobest::create_spatpos(
    id = 1:nrow(janno_final),
    x = janno_final$x, 
    y = janno_final$y, 
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = multivar_method_observation_bundles[[run]],
  kernel = mobest::create_kernset_cross(
    ds = 800*1000,
    dt = 800,
    g = 0.1 # TODO: Replace with appropriate nugget for each dimension
  ),
  iterations = 1,
  groups = 10,
  quiet = F
)

multivar_comparison <- multivar_comparison %>%
  dplyr::select(-ds, -dt, -g) %>%
  dplyr::mutate(
    multivar_method = multivar_method_permutations$method[[run]],
    multivar_fstate = multivar_method_permutations$fstate[[run]]
  )

#### save result current iteration ####

save(multivar_comparison, file = paste0(
  "data/parameter_exploration/multivariate_analysis_comparison/crossvalidation/multivar_comparison_",
  run,
  ".RData"
))
