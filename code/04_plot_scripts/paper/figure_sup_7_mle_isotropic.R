library(magrittr)
library(ggplot2)

load("data/parameter_exploration/targeted/mle_iso.RData")

mle_mean_across_iterations <- mle_iso %>%
  dplyr::group_by(dependent_var_id, scaling_factor_label) %>%
  dplyr::summarise(
    d = mean(d),
    l = mean(l),
    .groups = "drop"
  )

p1 <- mle_mean_across_iterations %>%
  ggplot() +
  geom_point(
    aes(x = scaling_factor_label, y = d),
  ) +
  geom_text(
    aes(x = scaling_factor_label, y = d, label = round(d)),
    angle = 90, hjust = 1.5, size = 3
  ) +
  facet_wrap(~dependent_var_id, scales = "free_y") +
  theme_bw() +
  ylab(latex2exp::TeX("$\\sqrt{\\theta}$")) +
  xlab("Scaling factor") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  )

p2 <- mle_mean_across_iterations %>%
  ggplot() +
  geom_point(
    aes(x = scaling_factor_label, y = l)
  ) +
  geom_text(
    aes(x = scaling_factor_label, y = l, label = round(l)),
    angle = 90, hjust = 1.5, size = 3
  ) +
  facet_wrap(~dependent_var_id, scales = "free_y") +
  theme_bw() +
  ylab("log likelihood") +
  xlab("scaling factor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p <- cowplot::plot_grid(p1, p2, nrow = 2, align = "v", axis = "lr", rel_heights = c(0.9, 1), labels = c("A", "B"))

ggsave(
  "plots/figure_sup_7_mle_isotropic.pdf",
  plot = p,
  device = "pdf",
  scale = 0.8,
  dpi = 300,
  width = 400, height = 150, units = "mm",
  limitsize = F
)

