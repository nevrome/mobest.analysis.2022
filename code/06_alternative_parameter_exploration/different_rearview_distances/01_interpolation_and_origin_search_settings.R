library(magrittr)

#### data ####

load("data/origin_search/default_kernel.RData")

temporal_kernel_size_years <- default_kernel[[1]]$d[[3]]

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

retrospection_distance_low <- kernel_theta_data %>%
  dplyr::filter(k < 0.75) %>%
  head(1) %$%
  dist_p1_p2

retrospection_distance_high <- kernel_theta_data %>%
  dplyr::filter(k < 0.25) %>%
  head(1) %$%
  dist_p1_p2

retrospection_distances <- c(
  low = retrospection_distance_low, 
  high = retrospection_distance_high
)

# library(ggplot2)
# kernel_theta_data %>%
#   ggplot() +
#   geom_line(
#     aes(dist_p1_p2, k)
#   ) +
#   geom_vline(xintercept = retrospection_distances)

save(retrospection_distances, file = "data/origin_search/retrospection_distance_retrovar.RData")
