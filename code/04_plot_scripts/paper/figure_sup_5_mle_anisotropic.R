library(magrittr)
library(ggplot2)

load("data/parameter_exploration/targeted/mle_ani.RData")

# number of non-converging runs
mle_ani$converged %>% table() %>% `/`(4) # divided by 4, because this is a long dataset - see $parameter

mle_ani_theta <- mle_ani %>%
  dplyr::filter(parameter != "g")

mle_ani_nugget <- mle_ani %>%
  dplyr::filter(parameter == "g")

levels(mle_ani_theta$parameter) <- levels(mle_ani_nugget$parameter) <- c(
  latex2exp::TeX("$\\sqrt{\\theta_x}$"), 
  latex2exp::TeX("$\\sqrt{\\theta_y}$"),
  latex2exp::TeX("$\\sqrt{\\theta_t}$"),
  latex2exp::TeX("$\\eta$")
)

p_theta <- mle_ani_theta %>%
  # remove non-convergence runs
  dplyr::filter(converged == 0) %>%
  ggplot() +
  facet_wrap(
    dependent_var_id~parameter,
    nrow = 3,
    ncol = 3,
    # rows = dplyr::vars(mle_method),
    # cols = dplyr::vars(parameter),
    scales = "free_y",
    labeller = label_parsed
  ) +
  geom_jitter(
    aes(x = mle_method, y = value, fill = mle_method),
    size = 2,
    height = 0,
    shape = 21
  ) +
  guides(fill = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  xlab("") +
  ylab(latex2exp::TeX("estimated $\\sqrt{\\theta_\\cdots}$"))# +
  # scale_y_continuous(sec.axis = sec_axis(~.^2, name = latex2exp::TeX("estimated $\\theta_\\cdots$")))

p_nugget <- mle_ani_nugget %>%
  # remove non-convergence runs
  dplyr::filter(converged == 0) %>%
  ggplot() +
  facet_wrap(
    dependent_var_id~parameter,
    nrow = 3,
    scales = "free_y",
    labeller = label_parsed
  ) +
  geom_jitter(
    aes(x = mle_method, y = value, fill = mle_method),
    size = 2,
    height = 0,
    shape = 21
  ) +
  guides(fill = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  xlab("") +
  ylab(latex2exp::TeX("estimated $\\eta$"))

p <- cowplot::plot_grid(p_theta, p_nugget, nrow = 1, rel_widths = c(0.71, 0.29))

ggsave(
  "plots/figure_sup_5_mle_anisotropic.pdf",
  plot = p,
  device = "pdf",
  scale = 0.6,
  dpi = 300,
  width = 320, height = 270, units = "mm",
  limitsize = F
)

