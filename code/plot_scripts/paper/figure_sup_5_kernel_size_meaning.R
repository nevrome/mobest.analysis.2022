library(magrittr)
library(ggplot2)

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

kernel_theta_data$d_label <- as.factor(kernel_theta_data$d)
levels(kernel_theta_data$d_label) <- c(
  latex2exp::TeX("$\\theta = 100,\\; \\sqrt{\\theta} = 10$"),
  latex2exp::TeX("$\\theta = 2000,\\; \\sqrt{\\theta} \\approx 44.7$"),
  latex2exp::TeX("$\\theta = 5000,\\; \\sqrt{\\theta} \\approx 70.7$"),
  latex2exp::TeX("$\\theta = 20000,\\; \\sqrt{\\theta} \\approx 141.4$")
)

p <- kernel_theta_data %>% 
  ggplot() +
  geom_line(
    aes(dist_p1_p2, k),
    size = 1
  ) +
  geom_vline(
    aes(xintercept = sqrt(d))
  ) +
  scale_color_viridis_c() +
  facet_wrap(
    ~d_label, 
    ncol = 2,  
    labeller = label_parsed
  ) +
  theme_bw() +
  xlab("pairwise distance") +
  ylab("Covariance K")

ggsave(
  "plots/figure_sup_5_kernel_size_meaning.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 150, units = "mm",
  limitsize = F
)

