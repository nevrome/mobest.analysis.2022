library(magrittr)

load("data/genotype_data/janno_final.RData")
load("data/genotype_data/multivar_perm_obs_bundles.RData")
source("code/02_parameter_estimation/01_compare_multivariate_analysis/distance_helper_functions.R")

spatiotemp_dist <- d(
  mobest::calculate_geo_pairwise_distances(
    ids = janno_final$Poseidon_ID,
    independent = mobest::create_spatpos(
      id = janno_final$Poseidon_ID,
      x = janno_final$x,
      y = janno_final$y,
      z = janno_final$Date_BC_AD_Median_Derived
    )
  )[,3],
  mobest::calculate_time_pairwise_distances(
    ids = janno_final$Poseidon_ID,
    independent = mobest::create_spatpos(
      id = janno_final$Poseidon_ID,
      x = janno_final$x,
      y = janno_final$y,
      z = janno_final$Date_BC_AD_Median_Derived
    )
  )[,3]
)

distance_products <- purrr::pmap_df(
  list(
    multivar_method_permutations$method,
    multivar_method_permutations$fstate,
    multivar_method_observation_bundles
  ),
  function(method, fstate, observation_bundle) {
    cum_across_dim_pairwise_distances <- mobest::calculate_dependent_pairwise_distances(
      ids = janno_final$Poseidon_ID,
      dependent = observation_bundle
    ) %>%
      purrr::reduce( function(x, y) { dplyr::bind_cols(x, y[3]) } ) %>%
      dplyr::select(-Var1, -Var2) %>%
      d_cum_df
    distance_median <- cum_across_dim_pairwise_distances %>%
      dplyr::summarise(
        dplyr::across(.fns = function(x) { median(x) } )
      )
    distance_correlation <- cum_across_dim_pairwise_distances %>%
      dplyr::summarise(
        dplyr::across(.fns = function(x) { cor(spatiotemp_dist, x)^2 } )
      )
    dplyr::bind_rows(distance_median, distance_correlation) %>%
      dplyr::mutate(
        measure = c("median", "distance correlation"),
        method = method,
        snp_selection = fstate
      )
  }
)

distance_median <- distance_products %>% dplyr::filter(measure == "median")
distance_correlation <- distance_products %>% dplyr::filter(measure == "distance correlation")

save(distance_median, file = "data/parameter_exploration/multivariate_analysis_comparison/distance_median.RData")

#save(large_distance_table, file = "data/parameter_exploration/multivariate_comparison/large_distance_table.RData")

# dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), round, 3)) %>% knitr::kable()
