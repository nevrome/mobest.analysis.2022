library(magrittr)

load("data/parameter_exploration/variogram/estimated_nuggets.RData")
load("data/parameter_exploration/crossvalidation/best_kernel.RData")
kern <- dplyr::left_join(
  best_kernel %>% dplyr::select(-mean_squared_difference),
  estimated_nuggets %>% dplyr::select(-dist_type),
  by = "dependent_var_id"
)

#### kernel size ####

default_kernset_mds2 <- mobest::create_kernset(
  C1_mds_u = mobest::create_kernel(
    dsx = kern$dsx[1] * 1000, dsy = kern$dsy[1] * 1000, dt = kern$dt[1],
    g = kern$nugget[1],
    on_residuals = T, auto = F
  ),
  C2_mds_u = mobest::create_kernel(
    dsx = kern$dsx[2] * 1000, dsy = kern$dsy[2] * 1000, dt = kern$dt[2],
    g = kern$nugget[2],
    on_residuals = T, auto = F
  )
)

save(default_kernset_mds2, file = "data/origin_search/default_kernset_mds2.RData")

default_kernset_mds3 <- mobest::create_kernset(
  C1_mds_u = mobest::create_kernel(
    dsx = kern$dsx[1] * 1000, dsy = kern$dsy[1] * 1000, dt = kern$dt[1],
    g = kern$nugget[1],
    on_residuals = T, auto = F
  ),
  C2_mds_u = mobest::create_kernel(
    dsx = kern$dsx[2] * 1000, dsy = kern$dsy[2] * 1000, dt = kern$dt[2],
    g = kern$nugget[2],
    on_residuals = T, auto = F
  ),
  C3_mds_u = mobest::create_kernel(
    dsx = kern$dsx[3] * 1000, dsy = kern$dsy[3] * 1000, dt = kern$dt[3],
    g = kern$nugget[3],
    on_residuals = T, auto = F
  )
)

save(default_kernset_mds3, file = "data/origin_search/default_kernset_mds3.RData")

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

retrospection_distance_low <- kernel_theta_data %>%
  dplyr::filter(k < 0.75) %>%
  head(1) %$%
  dist_p1_p2

retrospection_distance_default <- kernel_theta_data %>%
  dplyr::filter(k < 0.5) %>%
  head(1) %$%
  dist_p1_p2

retrospection_distance_high <- kernel_theta_data %>%
  dplyr::filter(k < 0.25) %>%
  head(1) %$%
  dist_p1_p2

retrospection_distances <- c(
  low = retrospection_distance_low,
  default = retrospection_distance_default,
  high = retrospection_distance_high
)

save(retrospection_distances, file = "data/origin_search/retrospection_distances.RData")
