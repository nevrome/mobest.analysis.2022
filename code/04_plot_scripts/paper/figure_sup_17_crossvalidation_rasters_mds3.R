library(magrittr)
library(ggplot2)

load("data/parameter_exploration/crossvalidation/interpol_comparison_group.RData")
load("data/parameter_exploration/crossvalidation/best_kernel.RData")

icg <- interpol_comparison_group %>% dplyr::group_split(dependent_var)
min_point <- best_kernel[[2]]

# for each ancestry component
ps <- lapply(seq_along(icg), function(i) {
  icg[[i]] %>%
    ggplot() +
    geom_tile(
      aes(x = ds, y = dt, fill = mean_squared_difference),
      colour = "grey20"
    ) +
    scale_fill_viridis_c(direction = -1) +
    facet_wrap(~dependent_var) +
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
    scale_y_continuous(breaks = seq(0, 2000, 100))
})

ps[[4]] <- ps[[4]] +
  geom_point(
    data = min_point,
    aes(x = ds, y = dt), 
    color = "red", pch = 4, size = 5
  ) +
  annotate(
    "text",
    x = min_point$ds + 500, y = min_point$dt,
    label = latex2exp::TeX(paste0("$\\sqrt{\\theta_s}$ = ", min_point$ds, " | \\sqrt{$\\theta_t}$ = ", min_point$dt)),
    parse = TRUE,
    color = "red",
    size = 4
  )

p <- cowplot::plot_grid(plotlist = ps, nrow = 2, ncol = 2)

ggsave(
  "plots/figure_sup_17_crossvalidation_rasters_mds3.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.8,
  dpi = 300,
  width = 350, height = 300, units = "mm",
  limitsize = F,
  bg = "white"
)
