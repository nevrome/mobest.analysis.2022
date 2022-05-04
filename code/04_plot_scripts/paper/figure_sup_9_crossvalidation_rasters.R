library(magrittr)
library(ggplot2)

load("data/parameter_exploration/crossvalidation/interpol_comparison_group.RData")
load("data/parameter_exploration/crossvalidation/best_kernel.RData")

# for each ancestry component
ps <- purrr::map2(
  interpol_comparison_group %>%
    dplyr::group_split(dependent_var_id),
  best_kernel %>%
    dplyr::group_split(dependent_var_id),
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
    scale_x_continuous(breaks = seq(0, 2000, 100)) +
    scale_y_continuous(breaks = seq(0, 2000, 100)) +
    geom_point(
      data = b,
      aes(x = dsx, y = dt), 
      color = "red", pch = 4, size = 5
    ) +
    annotate(
      "text",
      x = b$dsx + 500, y = b$dt,
      label = latex2exp::TeX(paste0("$\\sqrt{\\theta_s}$ = ", b$dsx, " | \\sqrt{$\\theta_t}$ = ", b$dt)),
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
  width = 350, height = 300, units = "mm",
  limitsize = F,
  bg = "white"
)
