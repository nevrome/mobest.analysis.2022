load("data/genotype_data/janno_final.RData")

d <- function(...) {
  in_vecs <- list(...)
  checkmate::assert_true(length(unique(purrr::map_int(in_vecs, length))) == 1)
  purrr::map(in_vecs, function(x) { x^2 }) %>% purrr::reduce(`+`) %>% sqrt
}

d_cum_df <- function(x) {
  purrr::map_dfc(1:ncol(x), function(i) {
      d_in_list <- if (i < 2) { list(x[,1]) } else { as.list(x[,1:i]) }
      dist_vec <- do.call(d, d_in_list)
      setNames(
        list(dist_vec),
        paste0("C1to", substr(colnames(x)[i], start = 1, stop = 2))
      )
    }
  )
}

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

mobest::calculate_dependent_pairwise_distances(
  ids = janno_final$Poseidon_ID,
  dependent = mobest::create_obs(
    C1_mds_u = janno_final$C1_mds_u,
    C2_mds_u = janno_final$C2_mds_u,
    C3_mds_u = janno_final$C3_mds_u,
    C4_mds_u = janno_final$C4_mds_u,
    C5_mds_u = janno_final$C5_mds_u
  )
) %>% 
  purrr::reduce(
    function(x, y) { dplyr::bind_cols(x, y[3]) }
  ) %>%
  dplyr::select(-Var1, -Var2) %>%
  d_cum_df %>%
  dplyr::summarise(
    dplyr::across(
      .fns = function(x) {
        cor(spatiotemp_dist, x)^2
      }
    )
  )

#save(large_distance_table, "data/parameter_exploration/multivariate_comparison/large_distance_table.RData")






