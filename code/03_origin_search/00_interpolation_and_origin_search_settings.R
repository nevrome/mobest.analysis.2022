library(magrittr)

load("data/parameter_exploration/crossvalidation_best_kernels.RData")

#### construct default kernel list ####

default_kernset <- purrr::pmap(
  as.list(crossvalidation_best_kernels), function(dsx, dsy, dt, g, ...) {
    mobest::create_kernel(
      dsx = dsx * 1000, dsy = dsy * 1000, dt = dt,
      g = g,
      on_residuals = T, auto = F
    )
  }
) %>%
  setNames(crossvalidation_best_kernels$dependent_var_id) %>%
  do.call(mobest::create_kernset, .)

save(default_kernset, file = "data/origin_search/default_kernset.RData")

#### retrospection_distance ####

temporal_kernel_size_years <- 800

kernel_theta <- Vectorize(function(distance, d) { exp(-(distance^2) / d) })

kernel_theta_data <- expand.grid(
  dist_p1_p2 = seq(0, 2000, 1),
  d = temporal_kernel_size_years^2
) %>%
  dplyr::mutate(
    k = kernel_theta(dist_p1_p2, d),
    k_cum = cumsum(k/sum(k))
  )

save(kernel_theta_data, file = "data/origin_search/kernel_theta_data.RData")

retrospection_distance_low <- kernel_theta_data %>%
  dplyr::filter(k < 0.8) %>%
  head(1) %$%
  dist_p1_p2

retrospection_distance_default <- kernel_theta_data %>%
  dplyr::filter(k < 0.5) %>%
  head(1) %$%
  dist_p1_p2

retrospection_distance_high <- kernel_theta_data %>%
  dplyr::filter(k < 0.2) %>%
  head(1) %$%
  dist_p1_p2

retrospection_distances <- c(
  low = retrospection_distance_low,
  default = retrospection_distance_default,
  high = retrospection_distance_high
)

save(retrospection_distances, file = "data/origin_search/retrospection_distances.RData")
