library(magrittr)
library(ggplot2)

load("data/parameter_exploration/crossvalidation_best_kernels.RData")
load("data/parameter_exploration/crossvalidation_kernel_comparison.RData")

# for each ancestry component
ps <- purrr::map2(
  crossvalidation_kernel_comparison %>%
    dplyr::filter(
      multivar_fstate == "u",
      multivar_method %in% c("mds", "pca_proj"),
      dim %in% c("C1", "C2", "C3")
    ) %>%
    dplyr::group_split(multivar_method, dim),
  crossvalidation_best_kernels %>%
    dplyr::filter(
      multivar_fstate == "u",
      multivar_method %in% c("mds", "pca_proj"),
      dim %in% c("C1", "C2", "C3")
    ) %>%
    dplyr::group_split(multivar_method, dim),
  function(a, b) {
    ggplot() +
    geom_tile(
      data = a,
      aes(x = dsx, y = dt, fill = mean_squared_difference)
    ) +
    scale_fill_viridis_c(direction = -1) +
    facet_wrap(~dependent_var_id) +
    coord_fixed() +
    theme_bw() +
    theme(
      legend.position = "right",
      axis.text = element_text(angle = 45, hjust = 1, size = 8)
    ) +
    guides(
      fill = guide_colorbar(title = "", barheight = 9)
    ) +
    xlab(latex2exp::TeX("$\\sqrt{\\theta_s}$")) +
    ylab(latex2exp::TeX("$\\sqrt{\\theta_t}$")) +
    scale_x_continuous(breaks = seq(0, 1500, 100)) +
    scale_y_continuous(breaks = seq(0, 1500, 100)) +
    geom_point(
      data = b,
      aes(x = dsx, y = dt), 
      color = "red", pch = 4, size = 5
    ) +
    annotate(
      "text",
      x = b$dsx + 300, y = b$dt,
      label = latex2exp::TeX(paste0("$$\\overset{\\sqrt{\\theta_s} = ", b$dsx, "}{\\sqrt{\\theta_t}$ = ", b$dt, "}")),
      parse = TRUE,
      color = "red",
      size = 4
    )
  }
)

p <- cowplot::plot_grid(plotlist = ps, nrow = 2, ncol = 3, labels = "AUTO")

ggsave(
  "plots/figure_sup_9_crossvalidation_rasters.pdf",
  plot = p,
  device = "pdf",
  scale = 0.8,
  dpi = 300,
  width = 450, height = 280, units = "mm",
  limitsize = F,
  bg = "white"
)
