load("data/simulation/scenarios.RData")
load("data/simulation/mock_data.RData")
load("data/simulation/example_run.RData")

ex1 <- mock_data_overview %>% 
  dplyr::filter(pop_size == 25, iteration == 6, process == "intertwined")

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

p_top <- cowplot::plot_grid(
  p_xy, p_xz, p_yz, nrow = 1
)
