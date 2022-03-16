source("code/04_plot_scripts/paper/mobility_curves_plot_function.R")

load("data/genotype_data/janno_final.RData")
load("data/origin_search/origin_grid_mean.RData")
load("data/origin_search/moving_origin_grid.RData")
load("data/origin_search/no_data_windows.RData")

p <- plot_curves(
  janno_final,
  origin_grid_mean,
  moving_origin_grid,
  no_data_windows
)

ggsave(
  paste0("plots/figure_5_mobility_curves.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 430, height = 300, units = "mm",
  limitsize = F
)
