source("code/04_plot_scripts/paper/mobility_curves_plot_function.R")

load("data/poseidon_data/janno_final.RData")
load("data/origin_search/origin_grid_derived_data_retro_low.RData")
list2env(origin_grid_derived_data, envir = .GlobalEnv)

p <- plot_curves(
  janno_final,
  origin_grid_mean,
  moving_origin_grid,
  no_data_windows
)

ggsave(
  paste0("plots/figure_sup_20_mobility_curves_retro_low.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 430, height = 300, units = "mm",
  limitsize = F
)
