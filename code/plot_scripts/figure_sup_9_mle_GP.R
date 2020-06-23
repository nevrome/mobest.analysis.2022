library(magrittr)
library(ggplot2)

load("data/parameter_exploration/mle/mle_out.RData")

p1 <- mleGP_out_df %>%
  tidyr::pivot_longer(cols = c(ds, dt), names_to = "d_type", values_to = "d_value") %>%
  ggplot() +
  geom_point(
    aes(x = scaling_factor_label, y = d_value, color = d_type),
  ) +
  theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(~sqrt(.), name = latex2exp::TeX("$\\sqrt{\\theta}$"))) +
  ylab(latex2exp::TeX("$\\theta$")) +
  xlab("Scaling factor")

p2 <- mleGP_out_df %>% ggplot() +
  geom_point(
    aes(x = scaling_factor_label, y = l)
  ) +
  theme_bw() +
  ylab("Gaussian process log likelihood") +
  xlab("Scaling factor")

cowplot::plot_grid(p1, p2, nrow = 2, align = "v", axis = "lr")