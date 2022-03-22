library(magrittr)

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

purrr::pmap_df(
  as.list(
    expand.grid(method = c("mds", "pca", "emu"), fstate = c("u", "f"), stringsAsFactors = F)
  ),
  function(method, fstate) {
    mobest::calculate_dependent_pairwise_distances(
      ids = janno_final$Poseidon_ID,
      dependent = mobest::create_obs(
        C1 = janno_final[[paste("C1", method, fstate, sep = "_")]],
        C2 = janno_final[[paste("C2", method, fstate, sep = "_")]],
        C3 = janno_final[[paste("C3", method, fstate, sep = "_")]],
        C4 = janno_final[[paste("C4", method, fstate, sep = "_")]],
        C5 = janno_final[[paste("C5", method, fstate, sep = "_")]]
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
      ) %>%
      dplyr::mutate(
        method = method,
        snp_selection = fstate
      )
  }
)




#save(large_distance_table, "data/parameter_exploration/multivariate_comparison/large_distance_table.RData")






