library(ggplot2)

load("data/simulation/permutations_accuracy_summary.RData")

kernel_labels <- c(
  `kernel_1` = "kernel d = 0.1",
  `kernel_2` = "kernel d = 0.2",
  `kernel_3` = "kernel d = 0.3",
  `kernel_4` = "kernel d = 0.4",
  `kernel_5` = "kernel d = 0.5"
)

pop_size_labels <- c(
  `10` = "pop size = 10*4",
  `50` = "pop size = 50*4",
  `100` = "pop size = 100*4"
)

p <- ggplot(data = permutations_accuracy_summary) +
  ggh4x::facet_nested(
    kernel_setting_id ~ pop_size + dependent_setting_id,
    labeller = labeller(
      kernel_setting_id = as_labeller(kernel_labels),
      pop_size = as_labeller(pop_size_labels)
    )
  ) +
  geom_line(aes(x = field_z, y = n_top_left)) +
  geom_point(aes(x = field_z, y = n_top_left))+ 
  geom_hline(yintercept = 25) +
  scale_x_continuous(breaks = seq(0.1,0.9,0.1)) +
  scale_y_continuous(breaks = seq(25, 100, 25)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.5)
  ) +
  ylab("number \"correct\" origin attributions")

ggsave(
  paste0("plots/figure_sup_31_simulation_accuracy_permutations.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.8,
  dpi = 300,
  width = 300, height = 200, units = "mm",
  limitsize = F
)

