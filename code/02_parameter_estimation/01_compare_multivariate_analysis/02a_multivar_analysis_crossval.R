library(magrittr)

#### load command line arguments ####

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
run <- as.integer(args[1])

#### load data ####

load("data/genotype_data/janno_final.RData")
load("data/genotype_data/multivar_perm_obs_bundles.RData")
load("data/parameter_exploration/multivariate_analysis_comparison/distance_products.RData")

method_run <- multivar_method_permutations$method[[run]]
fstate_run <- multivar_method_permutations$fstate[[run]]

distance_products_for_run <- distance_products %>%
  dplyr::filter(method == method_run, snp_selection == fstate_run) %>%
  dplyr::arrange(
    factor(dim, levels = names(multivar_method_observation_bundles[[run]]))
  )

kernels_per_dim_for_run <- mobest::create_kernset_multi(
  purrr::map(
    distance_products_for_run$estimated_nugget, function(g) { 
      mobest::create_kernel(
        800*1000,
        800*1000,
        800,
        g
      )
    }) %>% 
    magrittr::set_names(distance_products_for_run$dim) %>%
    do.call(mobest::create_kernset, .),
  .names = paste("kernel_settings", method_run, fstate_run, sep = "_")
)

#### run crossvalidation ####

multivar_comparison <- mobest::crossvalidate(
  independent = mobest::create_spatpos(
    id = 1:nrow(janno_final),
    x = janno_final$x, 
    y = janno_final$y, 
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = multivar_method_observation_bundles[[run]],
  kernel = kernels_per_dim_for_run,
  iterations = 1,
  groups = 10,
  quiet = F
)

multivar_comparison <- multivar_comparison %>%
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
