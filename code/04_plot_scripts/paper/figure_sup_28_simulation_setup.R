library(magrittr)
library(ggplot2)

load("data/simulation/mock_data.RData")

ex1 <- mock_data_overview %>% 
  dplyr::filter(pop_size == 50, iteration == 6, scenario == "limited_slow")

p_xy <- ggplot() +
  geom_point(
    data = ex1 %>% dplyr::arrange(z),
    mapping = aes(x, y, color = group)
  ) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none")

p_xz <- ggplot() +
  geom_point(
    data = ex1 %>% dplyr::arrange(y),
    mapping = aes(x, z, color = group)
  ) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none")

p_yz <- ggplot() +
  geom_point(
    data = ex1 %>% dplyr::arrange(x),
    mapping = aes(y, z, color = group)
  ) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none")

p <- cowplot::plot_grid(
  p_xy, p_xz, p_yz, nrow = 1
)

ggsave(
  paste0("plots/figure_sup_28_simulation_setup.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 300, height = 100, units = "mm",
  limitsize = F
)
