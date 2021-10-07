library(magrittr)

#### kernel size ####

spatial_kernel_size_km <- 500
temporal_kernel_size_years <- 900
nugget <- 0.06

default_kernel <- mobest::create_kernset_multi(
  d = list(c(
    spatial_kernel_size_km * 1000, 
    spatial_kernel_size_km * 1000, 
    temporal_kernel_size_years
  )), 
  g = nugget, 
  on_residuals = T, 
  auto = F,
  it = "ds500_dt900_g006"
)

save(default_kernel, file = "data/origin_search/default_kernel.RData")

#### retrospection_distance ####

kernel_theta <- Vectorize(function(distance, d) { exp(-(distance^2) / d) })

kernel_theta_data <- expand.grid(
  dist_p1_p2 = seq(0, 2000, 1),
  d          = temporal_kernel_size_years^2
) %>%
  dplyr::mutate(
    k = kernel_theta(dist_p1_p2, d),
    k_cum = cumsum(k/sum(k))
  )

save(kernel_theta_data, file = "data/origin_search/kernel_theta_data.RData")

retrospection_distance <- kernel_theta_data %>%
  dplyr::filter(k < 0.5) %>%
  head(1) %$%
  dist_p1_p2

save(retrospection_distance, file = "data/origin_search/retrospection_distance.RData")
