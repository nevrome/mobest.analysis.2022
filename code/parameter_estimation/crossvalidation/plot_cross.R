###

ggplot(interpol_comparison) +
  geom_histogram(
    mapping = aes(x = difference), bins = 100
  ) +
  facet_grid(rows = vars(dependent_var)) +
  # geom_vline(
  #   data = interpol_comparison_sd %>% dplyr::filter(!grepl("norm", PC)),
  #   mapping = aes(xintercept = sd_difference),
  #   color = "red"
  # ) +
  # geom_vline(
  #   mapping = aes(xintercept = 0),
  #   color = "black"
  # ) +
  # geom_vline(
  #   data = interpol_comparison_sd %>% dplyr::filter(!grepl("norm", PC)),
  #   mapping = aes(xintercept = -sd_difference),
  #   color = "red"
  # ) +
  # theme_bw() +
  xlim(-0.05, 0.05)
