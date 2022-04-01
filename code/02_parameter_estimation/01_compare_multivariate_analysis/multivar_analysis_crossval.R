library(magrittr)

#### load command line arguments ####

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
run <- args[1]

#### load data ####

load("data/genotype_data/janno_final.RData")

#### prepare method permutations ####

permutations <- as.list(
  expand.grid(
    method = c("mds", "pca", "emu", "pca_proj"),
    fstate = c("u", "f"),
    end_dimension_sequence = 10,
    stringsAsFactors = F
  )
)

observation_bundles_list <- permutations %>%
  purrr::pmap(
    function(method, fstate, end_dimension_sequence) {
      dims <- paste0("C", 1:end_dimension_sequence)
      dep_va_list <- paste(dims, method, fstate, sep = "_") %>%
        purrr::map( function(x) { janno_final[[x]] } )
      names(dep_va_list) <- dims
      do.call(mobest::create_obs, dep_va_list)
    }
  )

#### run crossvalidation ####

multivar_comparison <- mobest::crossvalidate(
  independent = mobest::create_spatpos(
    id = 1:nrow(janno_final),
    x = janno_final$x, 
    y = janno_final$y, 
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = observation_bundles_list[[run]],
  kernel = mobest::create_kernset_cross(
    ds = 500*1000,
    dt = 500, 
    g = 0.1
  ),
  iterations = 10,
  groups = 10,
  quiet = F
)

multivar_comparison <- multivar_comparison %>%
  dplyr::select(-ds, -dt, -g) %>%
  dplyr::mutate(
    multivar_method = permutations$method[[run]],
    multivar_fstate = permutations$fstate[[run]]
  )

#### save result current iteration ####

save(multivar_comparison, file = paste0(
  "data/parameter_exploration/multivariate_analysis_comparison/crossvalidation/multivar_comparison_",
  run,
  ".RData"
))
