source("code/04_plot_scripts/paper/mobility_curves_plot_function.R")

filter_setting <- function(x) {
  x %>%
    dplyr::filter(multivar_method == "pca5", search_time == -667) %>%
    dplyr::filter(region_id != "Other region")
}

p <- plot_curves(filter_settings)

ggsave(
  paste0("plots/figure_sup_19_mobility_curves_pca5_default.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.6,
  dpi = 300,
  width = 500, height = 350, units = "mm",
  limitsize = F
)
