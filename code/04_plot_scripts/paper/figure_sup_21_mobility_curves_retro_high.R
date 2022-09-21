source("code/04_plot_scripts/paper/mobility_curves_plot_function.R")

filter_setting <- function(x) {
  x %>%
    dplyr::filter(multivar_method == "mds2", search_time == -1015) %>% #-378   -1015    -667
    dplyr::filter(region_id != "Other region")
}

p <- plot_curves(filter_settings)

ggsave(
  paste0("plots/figure_sup_21_mobility_curves_mds2_high.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 430, height = 300, units = "mm",
  limitsize = F
)
