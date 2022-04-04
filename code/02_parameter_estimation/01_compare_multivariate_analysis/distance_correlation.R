library(magrittr)

load("data/genotype_data/janno_final.RData")

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

d <- function(...) {
  in_vecs <- list(...)
  checkmate::assert_true(length(unique(purrr::map_int(in_vecs, length))) == 1)
  purrr::map(in_vecs, function(x) { x^2 }) %>% purrr::reduce(`+`) %>% sqrt
}

d_cum_df <- function(x) {
  purrr::map_dfc(2:ncol(x), function(i) {
    d_in_list <- as.list(x[,1:i])
    dist_vec <- do.call(d, d_in_list)
    setNames(
      list(dist_vec),
      paste0("C1toC", readr::parse_number(colnames(x)[i]))
    )
  }
  )
}

distance_products <- purrr::pmap_df(
  list(permutations$method, permutations$fstate, observation_bundles_list),
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
