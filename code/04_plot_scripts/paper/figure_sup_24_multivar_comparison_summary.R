library(magrittr)
library(ggplot2)

load("data/parameter_exploration/distance_products.RData")
load("data/parameter_exploration/crossvalidation_multivar_comparison.RData")
load("data/parameter_exploration/crossvalidation_best_kernels.RData")

multivar_comparison_summary <- distance_products %>%
  dplyr::filter(multivar_method != "pca") %>%
  dplyr::left_join(
    crossvalidation_multivar_comparison,
    by = c(
      "multivar_method",
      "multivar_fstate",
      "dim_range"
    )
  ) %>%
  dplyr::left_join(
    crossvalidation_best_kernels,
    by = c(
      "dependent_var_id",
      "multivar_method",
      "multivar_fstate",
      "dim"
    )
  )

multivar_comparison_summary_adjusted <- multivar_comparison_summary %<>%
  dplyr::mutate(
    dim = factor(dim, paste0("C", 1:10))
  ) %>%
  dplyr::mutate(
    multivar_method = dplyr::recode(
      multivar_method,
      "emu" = "EMU",
      "mds" = "MDS",
      "pca" = "PCA",
      "pca_proj" = "PCA\n(projected)"
    ),
    multivar_fstate = dplyr::recode_factor(
      multivar_fstate,
      "u" = "unfiltered SNP set",
      "f" = "filtered SNP set"
    )
  )

common_elements <- list(
  scale_shape_manual(values = c(
    "unfiltered SNP set" = 15,
    "filtered SNP set" = 0
  )),
  theme_bw(),
  theme(
    legend.position = "none",
    axis.title.x = element_blank()
  )
)

p_nugget <- multivar_comparison_summary_adjusted %>%
  ggplot() +
  geom_point(
    aes(
      x = dim,
      y = estimated_nugget,
      color = multivar_method, shape = multivar_fstate,
      group = multivar_method
    ),
    position = position_dodge(width = 0.5), size = 2
  ) +
  common_elements +
  ylab(latex2exp::TeX("$\\eta$")) +
  ggtitle("Estimated nugget")

p_corr <- multivar_comparison_summary_adjusted %>%
  ggplot() +
  geom_point(
    aes(
      x = dim,
      y = r_squared_genetic_spatiotemporal_distance,
      color = multivar_method, shape = multivar_fstate,
      group = multivar_method
    ),
    position = position_dodge(width = 0.5), size = 2
  ) +
  common_elements +
  ylab(latex2exp::TeX("$R^2$")) +
  ggtitle("Correlation of spatiotemporal and genetic distance (CX means C1-CX)")

p_cross_diff <- multivar_comparison_summary_adjusted %>%
  ggplot() +
  geom_point(
    aes(
      x = dim,
      y = mean_squared_difference_estimated_real,
      color = multivar_method, shape = multivar_fstate
      ),
    position = position_dodge(width = 0.5), size = 2
  ) +
  common_elements +
  ylab("Difference") +
  ggtitle("Normalized difference between predicted and measured ancestry (CX means C1-CX)") +
  theme(legend.position = "right") +
  guides(
    color = guide_legend(title = "Method"),
    shape = guide_legend(title = "SNP selection")
  )

p_kernel_ds <- multivar_comparison_summary_adjusted %>%
  ggplot() +
  geom_point(
    aes(
      x = dim,
      y = dsx,
      color = multivar_method, shape = multivar_fstate
    ),
    position = position_dodge(width = 0.5), size = 2
  ) +
  common_elements +
  ylab(latex2exp::TeX("$\\sqrt{\\theta_s}$")) +
  ggtitle(latex2exp::TeX("estimated ideal $\\sqrt{\\theta_s}$")) +
  scale_y_continuous(breaks = seq(100, 2000, 200), limits = c(100, 1500))

p_kernel_dt <- multivar_comparison_summary_adjusted %>%
  ggplot() +
  geom_point(
    aes(
      x = dim,
      y = dt,
      color = multivar_method, shape = multivar_fstate
    ),
    position = position_dodge(width = 0.5), size = 2
  ) +
  common_elements +
  ylab(latex2exp::TeX("$\\sqrt{\\theta_t}$")) +
  ggtitle(latex2exp::TeX("estimated ideal $\\sqrt{\\theta_t}$")) +
  scale_y_continuous(breaks = seq(100, 2000, 200), limits = c(100, 1500))

p <- cowplot::plot_grid(
  p_nugget, p_corr, p_cross_diff, p_kernel_ds, p_kernel_dt,
  ncol = 1, align = "v", axis = "lr",
  labels = "AUTO"
)

ggsave(
  paste0("plots/figure_sup_24_multivar_comparison_summary2.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 380, height = 430, units = "mm",
  limitsize = F
)

