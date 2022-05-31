load("data/simulation/scenarios.RData")
load("data/simulation/mock_data.RData")

ex <- mock_data_overview %>% 
  dplyr::filter(pop_size == 25, iteration == 6, process == "intertwined")

p_xy <- ggplot() +
  geom_point(
    data = ex %>% dplyr::arrange(z),
    mapping = aes(x, y, color = group)
  ) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none")

p_xz <- ggplot() +
  geom_point(
    data = ex %>% dplyr::arrange(y),
    mapping = aes(x, z, color = group)
  ) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none")

p_yz <- ggplot() +
  geom_point(
    data = ex %>% dplyr::arrange(x),
    mapping = aes(y, z, color = group)
  ) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none")

p_top <- cowplot::plot_grid(
  p_xy, p_xz, p_yz, nrow = 1
)

p_scenarios <- scenario_blueprint %>%
  ggplot() +
  facet_wrap(~func) +
  geom_ribbon(aes(x, ymin = y_min, ymax = y_max, fill = group), alpha = 0.2) +
  geom_line(aes(x, y_mean, color = group)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("z (\"time\")") +
  ylab("component (\"ancestry\")")

cowplot::plot_grid(
  p_top, p_scenarios, nrow = 2
)

####




ggplot() +
  geom_point(
    data = ex1,
    mapping = aes(z, component, color = group)
  )

ex2 <- overview %>% dplyr::filter(iteration == 6)

ex2 %>%
  ggplot() +
  geom_point(
    mapping = aes(z, component, color = group)
  ) +
  # geom_smooth(
  #   mapping = aes(z, component, color = group)
  # ) +
  facet_grid(rows = dplyr::vars(process), cols = dplyr::vars(pop_size))
