#scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/parameter_exploration/mle/mlesep_out.RData data/parameter_exploration/mle/mlesep_out.RData

library(magrittr)
library(ggplot2)

load("data/parameter_exploration/mle/mlesep_out.RData")

# number of non-converging runs
mlesep_out$conv %>% table() %>% `/`(4)

mlesep_out_theta <- mlesep_out %>% 
  dplyr::filter(parameter != "g")

mlesep_out_nugget <- mlesep_out %>% 
  dplyr::filter(parameter == "g")

levels(mlesep_out_theta$parameter) <- levels(mlesep_out_nugget$parameter) <- c(
  latex2exp::TeX("$\\theta_x$"), 
  latex2exp::TeX("$\\theta_y$"),
  latex2exp::TeX("$\\theta_t$"),
  latex2exp::TeX("$\\eta$")
)

p_theta <- mlesep_out_theta %>%
  # remove non-convergence runs
  dplyr::filter(conv == 0) %>%
  ggplot() +
  facet_wrap(
    mle_method~parameter,
    nrow = 2,
    ncol = 3,
    # rows = dplyr::vars(mle_method),
    # cols = dplyr::vars(parameter),
    scales = "free_y",
    labeller = label_parsed
  ) +
  geom_jitter(
    aes(x = ancestry_component, y = value, color = ancestry_component),
    size = 0.01,
    height = 0
  ) +
  guides(color = F) +
  theme_bw() +
  xlab("ancestry_component") +
  ylab(latex2exp::TeX("estimated $\\theta_\\cdots$")) +
  scale_y_continuous(sec.axis = sec_axis(~sqrt(.), name = latex2exp::TeX("estimated $\\sqrt{\\theta_\\cdots}$")))

p_nugget <- mlesep_out_nugget %>%
  # remove non-convergence runs
  dplyr::filter(conv == 0) %>%
  ggplot() +
  facet_wrap(
    mle_method~parameter,
    nrow = 2,
    scales = "free_y",
    labeller = label_parsed
  ) +
  geom_jitter(
    aes(x = ancestry_component, y = value, color = ancestry_component),
    size = 0.01,
    height = 0
  ) +
  guides(color = F) +
  theme_bw() +
  xlab("ancestry_component") +
  ylab(latex2exp::TeX("estimated $\\eta$"))

p <- cowplot::plot_grid(p_theta, p_nugget, nrow = 1, rel_widths = c(0.75, 0.25))

ggsave(
  "plots/figure_sup_4_mle_GPsep.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 200, units = "mm",
  limitsize = F
)

