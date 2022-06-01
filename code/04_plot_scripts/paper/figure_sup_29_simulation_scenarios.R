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

ex2 <- mock_data_overview %>% 
  dplyr::filter(pop_size == 25, iteration == 6)

ggplot() +
  facet_grid(
    rows = dplyr::vars(kernel_setting_id),
    cols = dplyr::vars(process)) +
  geom_ribbon(
    data = interpol_test_res %>% dplyr::rename(process = dependent_setting_id),
    aes(z, ymin = mean-sd, ymax = mean+sd, fill = pred_grid_id),
    alpha = 0.2
  ) +
  geom_line(
    data = interpol_test_res %>% dplyr::rename(process = dependent_setting_id),
    aes(z, mean, color = pred_grid_id)
  ) +
  geom_point(
    data = ex2,
    mapping = aes(z, component, color = group)
  ) +
  coord_cartesian(ylim = c(0, 1))
