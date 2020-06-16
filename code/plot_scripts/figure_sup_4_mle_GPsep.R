#scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/parameter_exploration/mle_out.RData ../parameter_exploration/mle_out.RData

library(magrittr)
library(ggplot2)

load("data/parameter_exploration/mle_out.RData")

mle_out %>%
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
    aes(x = ancestry_component, y = value, color = ancestry_component)
  ) +
  guides(color = F) +
  theme_bw() +
  xlab("ancestry_component") +
  ylab("estimated value")
