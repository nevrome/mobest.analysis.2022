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

ex2 <- mock_data_overview %>% 
  dplyr::filter(pop_size == 25, iteration == 6)

ggplot() +
  facet_wrap(~process) +
  geom_point(
    data = ex2,
    mapping = aes(z, component, color = group)
  ) +
  geom_line(
    data = interpol_test_res %>% dplyr::rename(process = dependent_setting_id),
    aes(z, mean, color = pred_grid_id)
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


####


p_list <- purrr::map2(
  dplyr::group_split(locate_test_product, dependent_setting_id),
  dplyr::group_split(ovs, dependent_setting_id),
  function(locate_dep, ovs_dep) {
    ggplot() +
      facet_wrap(~field_z) +
      geom_raster(
        data = locate_dep,
        mapping = aes(x = field_x, y = field_y, fill = probability)
      ) +
      geom_point(
        data = mobest::create_spatpos(id = "pioneer", x = 0.75, y = 0.25, z = 1),
        mapping = aes(x = x, y = y),
        colour = "red"
      ) +
      geom_point(
        data = ovs_dep,
        mapping = aes(x = field_x, y = field_y),
        colour = "orange"
      ) +
      coord_fixed() +
      theme_bw() +
      theme(legend.position = "none")
  }
)

cowplot::plot_grid(
  plotlist = p_list, nrow = 1
)
