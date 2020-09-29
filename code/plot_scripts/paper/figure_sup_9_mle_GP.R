library(magrittr)
library(ggplot2)

load("data/parameter_exploration/mle/mle_out.RData")

p1 <- mle_out %>%
  ggplot() +
  geom_point(
    aes(x = scaling_factor_label, y = d),
  ) +
  geom_text(
    aes(x = scaling_factor_label, y = d, label = round(d)),
    angle = 90, hjust = 1.5, size = 3
  ) +
  facet_wrap(~ancestry_component, scales = "free_y") +
  theme_bw() +
  ylab(latex2exp::TeX("$\\sqrt{\\theta}$")) +
  xlab("Scaling factor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- mle_out %>% ggplot() +
  geom_point(
    aes(x = scaling_factor_label, y = l)
  ) +
  geom_text(
    aes(x = scaling_factor_label, y = l, label = round(l)),
    angle = 90, hjust = 1.5, size = 3
  ) +
  facet_wrap(~ancestry_component, scales = "free_y") +
  theme_bw() +
  ylab("Gaussian process log likelihood") +
  xlab("Scaling factor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p <- cowplot::plot_grid(p1, p2, nrow = 2, align = "v", axis = "lr")

ggsave(
  "plots/figure_sup_9_mle_GP.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.8,
  dpi = 300,
  width = 300, height = 150, units = "mm",
  limitsize = F
)

