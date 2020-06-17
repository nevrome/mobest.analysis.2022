library(magrittr)
library(ggplot2)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final

kernel_theta <- function(distance, d) { exp(-(distance^2) / d) }

kernel_theta_data <- expand.grid(
  dist_p1_p2 = seq(0, 300, 1),
  d = c(100, 2000, 5000, 20000)
)

kernel_theta_data$k <- sapply(
  1:nrow(kernel_theta_data), function(i, kernel_theta_data) {
    kernel_theta(kernel_theta_data$dist_p1_p2[i], kernel_theta_data$d[i])
  },
  kernel_theta_data
)

p <- kernel_theta_data %>% 
  ggplot() +
  geom_line(
    aes(dist_p1_p2, k, group = d),
    size = 1
  ) +
  scale_color_viridis_c() +
  facet_wrap(~d, ncol = 1) +
  theme_bw() +
  xlab("pairwise distance") +
  ylab("Covariance K")

ggsave(
  "plots/figure_sup_5_kernel_size_meaning.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 200, units = "mm",
  limitsize = F
)

