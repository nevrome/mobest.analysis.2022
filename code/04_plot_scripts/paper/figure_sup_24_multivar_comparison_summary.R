library(magrittr)
library(ggplot2)

load("data/parameter_exploration/multivariate_analysis_comparison/multivar_comparison_summary.RData")

multivar_comparison_summary_adjusted <- multivar_comparison_summary %<>%
  dplyr::mutate(
    dim = factor(dim, paste0("C", 1:10))
  ) %>%
  dplyr::mutate(
    method = dplyr::recode(
      method,
      "emu" = "EMU",
      "mds" = "MDS",
      "pca" = "PCA",
      "pca_proj" = "PCA\n(projected)"
    ),
    snp_selection = dplyr::recode_factor(
      snp_selection,
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

library(ggplot2)
p_nugget <- multivar_comparison_summary_adjusted %>%
  ggplot() +
  geom_point(
    aes(
      x = dim,
      y = estimated_nugget,
      color = method, shape = snp_selection,
      group = method
    ),
    position = position_dodge(width = 0.5)
  ) +
  common_elements +
  scale_y_reverse() +
  ylab("Estimated nugget\n")

p_corr <- multivar_comparison_summary_adjusted %>%
  ggplot() +
  geom_point(
    aes(
      x = dim,
      y = r_squared_genetic_spatiotemporal_distance,
      color = method, shape = snp_selection,
      group = method
    ),
    position = position_dodge(width = 0.5)
  ) +
  common_elements +
  theme(legend.position = "right") +
  ylab("R-Squared: Correlation of\nspatiotemporal and genetic distance") +
  guides(
    color = guide_legend(title = "Method"),
    shape = guide_legend(title = "SNP selection")
  )

p_cross_diff <- multivar_comparison_summary_adjusted %>%
  ggplot() +
  geom_point(
    aes(
      x = dim,
      y = mean_squared_difference_estimated_real,
      color = method, shape = snp_selection
      ),
    position = position_dodge(width = 0.5)
  ) +
  common_elements +
  scale_y_reverse() +
  ylab("Normalized difference between\npredicted and measured ancestry")

p <- cowplot::plot_grid(
  p_nugget, p_corr, p_cross_diff,
  ncol = 1, align = "v", axis = "lr",
  labels = "AUTO"
)

ggsave(
  paste0("plots/figure_sup_24_multivar_comparison_summary.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.6,
  dpi = 300,
  width = 400, height = 350, units = "mm",
  limitsize = F
)

