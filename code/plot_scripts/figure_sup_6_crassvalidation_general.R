library(magrittr)
library(ggplot2)

load("data/parameter_exploration/crossvalidation/interpol_comparison.RData")

sample_interpol_comparison <- interpol_comparison %>%
  dplyr::sample_n(10000)

sample_interpol_comparison %>%
  ggplot(aes(y = dependent_var, x = difference, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  ggridges::stat_density_ridges(
    geom = "density_ridges_gradient", 
    calc_ecdf = TRUE
  ) +
  viridis::scale_fill_viridis(name = "Tail probability") +
  geom_vline(
    mapping = aes(xintercept = 0),
    color = "red"
  ) +
  theme_bw() +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0.5))) +
  scale_x_continuous(limits = c(-0.05, 0.05))
