library(ggplot2)

load("data/simulation/permutations_accuracy_summary.RData")

ggplot(data = permutations_accuracy_summary) +
  ggh4x::facet_nested(kernel_length ~ pop_size + dependent_setting_id) +
  geom_line(aes(x = field_z, y = n_top_left)) +
  geom_point(aes(x = field_z, y = n_top_left))+ 
  geom_hline(yintercept = nr_iterations/4) +
  scale_x_continuous(breaks = seq(0,1,0.2)) +
  #scale_y_continuous(breaks = seq(25, 100, 25)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )# +
#coord_cartesian(ylim = c(25,100))