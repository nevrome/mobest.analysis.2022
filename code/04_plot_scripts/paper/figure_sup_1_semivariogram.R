library(ggplot2)
library(magrittr)

load("data/parameter_exploration/variogram/binned_distances.RData")

d_binned_long <- d_binned %>%
  tidyr::pivot_longer(
    cols = tidyselect::one_of(c("C1_dist", "C2_dist", "C1_dist_resid", "C2_dist_resid")),
    names_to = "distance_type", values_to = "distance_value"
  ) %>%
  dplyr::mutate(
    detrended = ifelse(
      grepl("resid", distance_type), 
      "detrended (residuals)", "not detrended"
    ),
    distance_type = sub("_resid", "", distance_type),
    distance_type = sub("_dist", "", distance_type),
    distance_type = factor(distance_type, levels = c("C1", "C2"))
  ) %>%
  dplyr::mutate(
    detrended = factor(detrended, levels = c("not detrended", "detrended (residuals)"))
  )

# plot loop
ps <- lapply(
  d_binned_long %>% 
    dplyr::filter(detrended == "detrended (residuals)") %>%
    dplyr::group_split(distance_type), 
  
  function(x) {
  
  ggplot(x) + 
    geom_raster(
      aes(
        x = geo_dist_cut,
        y = time_dist_cut,
        fill = distance_value
      )
    ) +
    facet_grid(cols = dplyr::vars(distance_type)) +
    scale_fill_viridis_c(direction = -1) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 7, angle = 45, hjust = 0.9)
    ) +
    guides(
      fill = guide_colorbar(title = "distance:", barwidth = 6)
    ) +
    xlab("spatial distance: 100km bins") +
    ylab("temporal distance: 100y bins")

})
  
p <- cowplot::plot_grid(plotlist = ps, nrow = 1, ncol = 2)

ggsave(
  "plots/figure_sup_1_semivariogram.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.4,
  dpi = 300,
  width = 500, height = 300, units = "mm",
  limitsize = F
)
