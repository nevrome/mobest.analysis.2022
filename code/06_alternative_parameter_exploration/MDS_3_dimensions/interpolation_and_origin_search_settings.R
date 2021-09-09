#### kernel size ####

spatial_kernel_size_km <- 400
temporal_kernel_size_years <- 800
nugget <- 0.2

default_kernel <- mobest::create_kernset_multi(
  d = list(c(
    spatial_kernel_size_km * 1000, 
    spatial_kernel_size_km * 1000, 
    temporal_kernel_size_years
  )), 
  g = nugget, 
  on_residuals = T, 
  auto = F,
  it = "ds400_dt800_g020"
)

save(default_kernel, file = "data/origin_search/default_kernel_mds3.RData")

#### retrospection_distance ####

kernel_theta <- Vectorize(function(distance, d) { exp(-(distance^2) / d) })

kernel_theta_data <- expand.grid(
  dist_p1_p2 = seq(0, 2000, 1),
  d = temporal_kernel_size_years^2
) %>%
  dplyr::mutate(
    k = kernel_theta(dist_p1_p2, d),
    k_cum = cumsum(k/sum(k))
  )

# library(ggplot2)
# kernel_theta_data %>%
#   ggplot() +
#   geom_line(
#     aes(dist_p1_p2, k)
#   ) +
#   geom_line(
#     aes(dist_p1_p2, k_cum)
#   ) +
#   geom_vline(
#     xintercept = kernel_theta_data %>%
#       dplyr::filter(k_cum > 0.682) %>%
#       head(1) %$%
#       dist_p1_p2
#   ) +
#   geom_vline(
#     xintercept = kernel_theta_data %>%
#       dplyr::filter(k < 0.5) %>%
#       head(1) %$%
#       dist_p1_p2
#   )

retrospection_distance <- kernel_theta_data %>%
  dplyr::filter(k < 0.5) %>%
  head(1) %$%
  dist_p1_p2

save(retrospection_distance, file = "data/origin_search/retrospection_distance_mds3.RData")
