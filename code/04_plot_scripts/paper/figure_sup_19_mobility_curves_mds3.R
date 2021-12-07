source("code/04_plot_scripts/paper/mobility_curves_plot_function.R")

load("data/poseidon_data/janno_final.RData")
load("data/origin_search/origin_grid_mean_mds3.RData")
load("data/origin_search/moving_origin_grid_mds3.RData")
load("data/origin_search/no_data_windows_mds3.RData")

p <- plot_curves(
  janno_final,
  origin_grid_mean,
  moving_origin_grid,
  no_data_windows
)

ggsave(
  paste0("plots/figure_sup_19_mobility_curves_mds3.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 430, height = 300, units = "mm",
  limitsize = F
)
