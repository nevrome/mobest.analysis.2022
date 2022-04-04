# calculate squared Euclidean distances
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