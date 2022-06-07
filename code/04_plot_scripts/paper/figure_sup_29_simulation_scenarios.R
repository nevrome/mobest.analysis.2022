library(magrittr)
library(ggplot2)

load("data/simulation/scenarios.RData")
load("data/simulation/mock_data.RData")
load("data/simulation/example_run.RData")

p_scenarios <- scenario_blueprint %>%
  ggplot() +
  facet_wrap(~scenario) +
  geom_ribbon(aes(x, ymin = y_min, ymax = y_max, fill = group), alpha = 0.2) +
  geom_line(aes(x, y_mean, color = group)) +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_blank()) +
  xlab("z (\"time\")") +
  ylab("component (\"ancestry\")")

ex2 <- mock_data_overview %>%
  dplyr::filter(pop_size == 50, iteration == 6)

interpol <- interpol_test_res %>% dplyr::mutate(
    scenario = dependent_setting_id
  )

kernel_labels <- c(
  `kernel_1` = "kernel d = 0.1",
  `kernel_2` = "kernel d = 0.2",
  `kernel_3` = "kernel d = 0.3",
  `kernel_4` = "kernel d = 0.4",
  `kernel_5` = "kernel d = 0.5"
)

p_example_runs <- ggplot() +
  facet_grid(
    rows = dplyr::vars(kernel_setting_id),
    cols = dplyr::vars(scenario),
    labeller = labeller(
      kernel_setting_id = as_labeller(kernel_labels)
    )
  ) +
  geom_ribbon(
    data = interpol,
    aes(z, ymin = mean-sd, ymax = mean+sd, fill = pred_grid_id),
    alpha = 0.2
  ) +
  geom_line(
    data = interpol,
    aes(z, mean, color = pred_grid_id)
  ) +
  geom_point(
    data = ex2,
    mapping = aes(z, component, color = group),
    size = 0.7,
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("z (\"time\")") +
  ylab("component (\"ancestry\")")

p <- cowplot::plot_grid(
  p_scenarios, p_example_runs, nrow = 2, axis = "lr" , align = "v",
  labels = c("A", "B"),
  rel_heights = c(1, 4)
)

ggsave(
  paste0("plots/figure_sup_29_simulation_scenarios.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 300, height = 400, units = "mm",
  limitsize = F
)

