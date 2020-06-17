#scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/parameter_exploration/mle_out.RData ../parameter_exploration/mle_out.RData

library(magrittr)
library(ggplot2)

load("data/parameter_exploration/mle_out.RData")

p <- mle_out %>%
  # remove non-convergence runs
  dplyr::filter(conv == 0) %>%
  ggplot() +
  facet_wrap(
    mle_method~parameter,
    nrow = 2,
    ncol = 4,
    # rows = dplyr::vars(mle_method),
    # cols = dplyr::vars(parameter),
    scales = "free_y"
  ) +
  geom_jitter(
    aes(x = ancestry_component, y = value, color = ancestry_component),
    size = 0.01,
    height = 0
  ) +
  guides(color = F) +
  theme_bw() +
  xlab("ancestry_component") +
  ylab("estimated value")

ggsave(
  "plots/figure_sup_4_mle_GPsep.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 200, units = "mm",
  limitsize = F
)

