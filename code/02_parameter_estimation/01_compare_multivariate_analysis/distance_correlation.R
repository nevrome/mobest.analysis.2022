library(magrittr)

load("data/genotype_data/janno_final.RData")
load("data/genotype_data/multivar_perm_obs_bundles.RData")
source("code/02_parameter_estimation/01_compare_multivariate_analysis/distance_helper_functions.R")

geo_pairwise_distances <- mobest::calculate_geo_pairwise_distances(
  ids = janno_final$Poseidon_ID,
  independent = mobest::create_spatpos(
    id = janno_final$Poseidon_ID,
    x = janno_final$x,
    y = janno_final$y,
    z = janno_final$Date_BC_AD_Median_Derived
  )
)

time_pairwise_distances <- mobest::calculate_time_pairwise_distances(
  ids = janno_final$Poseidon_ID,
  independent = mobest::create_spatpos(
    id = janno_final$Poseidon_ID,
    x = janno_final$x,
    y = janno_final$y,
    z = janno_final$Date_BC_AD_Median_Derived
  )
)

small_spatiotemporal_dists <- dplyr::full_join(
  geo_pairwise_distances, time_pairwise_distances, by = c("Var1", "Var2")
) %>% 
  dplyr::filter(time_dist < 50 & geo_dist < 50) %>%
  dplyr::filter(Var1 != Var2) %>%
  # remove duplicates
  igraph::graph_from_data_frame(directed = F) %>%
  igraph::simplify(remove.multiple = T, remove.loops = F) %>%
  igraph::as_data_frame(what = "edges")

spatiotemp_dist <- d(geo_pairwise_distances[,3], time_pairwise_distances[,3])

distance_products <- purrr::pmap_df(
  list(
    multivar_method_permutations$method,
    multivar_method_permutations$fstate,
    multivar_method_observation_bundles
  ),
  function(method, fstate, observation_bundle) {
    # calculate pairwise distances
    dim_pairwise_distances_all <- mobest::calculate_dependent_pairwise_distances(
      ids = janno_final$Poseidon_ID,
      dependent = observation_bundle,
      with_resid = T,
      independent = mobest::create_spatpos(
        id = janno_final$Poseidon_ID,
        x = janno_final$x,
        y = janno_final$y,
        z = janno_final$Date_BC_AD_Median_Derived
      )
    ) %>%
      purrr::reduce( function(x, y) { dplyr::bind_cols(x, y[3:4]) } )
    # split normal distances and residual distances
    dim_pairwise_distances <- dim_pairwise_distances_all %>%
      dplyr::select(-Var1, -Var2, -tidyselect::contains("resid"))
    dim_pairwise_distances_resid <- dim_pairwise_distances_all %>%
      dplyr::select(Var1, Var2, tidyselect::contains("resid"))
    # Nugget determination via variogram
    lower_left_distances <- small_spatiotemporal_dists %>%
      dplyr::left_join(
        dim_pairwise_distances_resid,
        by = c("from" = "Var1", "to" = "Var2")
      )
    
    variances <- janno_final %>%
      dplyr::select(tidyselect::ends_with(paste0("_", method, "_", fstate))) %>%
      tidyr::pivot_longer(
        cols = tidyselect::everything(),
        names_to = "dim_method_fstate"
      ) %>%
      dplyr::group_by(dim_method_fstate) %>%
      dplyr::summarise(variance = var(value)) %>%
      dplyr::mutate(
        dim = purrr::map_chr(strsplit(dim_method_fstate, "_"), function(x) { x[[1]] })
      )
      
    hu <- lower_left_distances %>% tidyr::pivot_longer(
        cols = tidyselect::contains("resid"),
        names_to = "dist_type", values_to = "dist_val"
      ) %>%
      dplyr::mutate(
        dim = purrr::map_chr(strsplit(dist_type, "_"), function(x) { x[[1]] })
      )
      
    hu %>%
      dplyr::left_join(variances, by = "dim") %>%
      dplyr::mutate(
        dist_val_adjusted = 0.5*(dist_val^2/variance)
      ) %>%
      dplyr::group_by(dim) %>%
      dplyr::summarise(
        estimated_nugget = mean(dist_val_adjusted)
      )
    
    # prepare cum: cumulation here means "distances in multi-dim spaces"
    cum_across_dim_pairwise_distances <- d_cum_df(dim_pairwise_distances)
    # cum: median pairwise distances in different genetic spaces
    distance_median <- cum_across_dim_pairwise_distances %>%
      dplyr::summarise(
        dplyr::across(.fns = function(x) { median(x) } )
      )
    # cum: correlation of different pairwise spaces with spatiotemporal distance
    distance_correlation <- cum_across_dim_pairwise_distances %>%
      dplyr::summarise(
        dplyr::across(.fns = function(x) { cor(spatiotemp_dist, x)^2 } )
      )
    # combine results in a common output data structure
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
