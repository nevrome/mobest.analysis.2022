library(magrittr)

load("data/parameter_exploration/crossvalidation/best_kernel.RData")
best_kernel_mds2 <- best_kernel[[2]]

#### kernel size ####

spatial_kernel_size_km <- best_kernel_mds2$ds
temporal_kernel_size_years <- best_kernel_mds2$dt
nugget <- best_kernel_mds2$g

default_kernel <- mobest::create_kernset_multi(
  d = list(c(
    spatial_kernel_size_km * 1000, 
    spatial_kernel_size_km * 1000, 
    temporal_kernel_size_years
  )), 
  g = nugget, 
  on_residuals = T, 
  auto = F,
  it = paste0(
    "ds", spatial_kernel_size_km, 
    "_dt", temporal_kernel_size_years, 
    "_g", as.character(nugget) %>% gsub("\\.", "", .)
  )
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

library(ggplot2)
kernel_theta_data %>%
  ggplot() +
  geom_line(
    aes(dist_p1_p2, k)
  ) +
  geom_vline(
    xintercept = kernel_theta_data %>%
      dplyr::filter(k < 0.5) %>%
      head(1) %$%
      dist_p1_p2
  )

retrospection_distance <- kernel_theta_data %>%
  dplyr::filter(k < 0.5) %>%
  head(1) %$%
  dist_p1_p2

save(retrospection_distance, file = "data/origin_search/retrospection_distance_mds3.RData")
