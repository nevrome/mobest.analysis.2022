library(magrittr)
library(ggplot2)

kernel_theta <- Vectorize(function(distance, d) { exp(-(distance^2) / d) })

kernel_theta_data <- expand.grid(
  dist_p1_p2 = seq(0, 2000, 1),
  d          = c(100, 500, 1000, 2000)^2
) %>%
  dplyr::mutate(
    k = kernel_theta(dist_p1_p2, d)
  )

kernel_theta_data$d_label <- as.factor(kernel_theta_data$d)
levels(kernel_theta_data$d_label) <- c(
  latex2exp::TeX("$\\theta = 10000,\\; \\sqrt{\\theta} = 100$"),
  latex2exp::TeX("$\\theta = 250000,\\; \\sqrt{\\theta} \\approx 500$"),
  latex2exp::TeX("$\\theta = 1000000,\\; \\sqrt{\\theta} \\approx 1000$"),
  latex2exp::TeX("$\\theta = 4000000,\\; \\sqrt{\\theta} \\approx 2000$")
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
  "plots/figure_sup_6_kernel_size_meaning.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 150, units = "mm",
  limitsize = F
)

