library(magrittr)
library(ggplot2)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final




kernel_theta <- function(distance, d) { exp(-(distance^2) / d) }

kernel_theta_data <- expand.grid(
  dist_p1_p2 = seq(0, 8000, 100),
  d = c(100, 5000, 10000)
)

kernel_theta_data$k <- sapply(
  1:nrow(kernel_theta_data), function(i, kernel_theta_data) {
    kernel_theta(kernel_theta_data$dist_p1_p2[i], kernel_theta_data$d[i])
  },
  kernel_theta_data
)

kernel_theta_data %>% 
  ggplot() +
  geom_line(
    aes(dist_p1_p2, k, group = d),
    size = 1
  ) +
  scale_color_viridis_c() +
  facet_wrap(~d, ncol = 1) +
  geom_vline(
    xintercept = anno$x %>% range %>% dist %>% as.vector() %>% `/`(1000)
  ) +
  geom_vline(
    xintercept = anno$y %>% range %>% dist %>% as.vector() %>% `/`(1000)
  ) +
  geom_vline(
    xintercept = anno$calage_center %>% range %>% dist %>% as.vector()
  )

