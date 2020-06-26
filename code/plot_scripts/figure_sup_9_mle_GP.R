library(magrittr)
library(ggplot2)

load("data/parameter_exploration/mle/mle_out.RData")

p1 <- mle_out %>%
  #tidyr::pivot_longer(cols = c(ds, dt), names_to = "d_type", values_to = "d_value") %>%
  ggplot() +
  geom_point(
    aes(x = scaling_factor_label, y = d),
  ) +
  theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(~sqrt(.), name = latex2exp::TeX("$\\sqrt{\\theta}$"))) +
  ylab(latex2exp::TeX("$\\theta$")) +
  xlab("")# +
  #scale_color_discrete(
  #  name = latex2exp::TeX("$\\theta$ scaling"), labels = list(latex2exp::TeX("$\\theta_s$"), latex2exp::TeX("$\\theta_t$"))
  #)

p2 <- mle_out %>% ggplot() +
  geom_point(
    aes(x = scaling_factor_label, y = l)
  ) +
  theme_bw() +
  ylab("Gaussian process log likelihood") +
  xlab("Scaling factor")

p <- cowplot::plot_grid(p1, p2, nrow = 2, align = "v", axis = "lr")

ggsave(
  "plots/figure_sup_9_mle_GP.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 150, units = "mm",
  limitsize = F
)
