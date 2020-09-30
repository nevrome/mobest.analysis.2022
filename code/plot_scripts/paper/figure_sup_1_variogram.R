library(ggplot2)
library(magrittr)

load("data/parameter_exploration/variogram/all_distances.RData")

# binning
d_binned <- d_all %>%
  dplyr::mutate(
    geo_dist_cut = (cut(
      geo_dist, breaks = c(seq(0, max(geo_dist), 100), max(geo_dist)), 
      include.lowest	= T, labels = F
    ) * 100) - 50,
    time_dist_cut = (cut(
      time_dist, breaks = c(seq(0, max(time_dist), 100), max(time_dist)), 
      include.lowest	= T, labels = F
    ) * 100) - 50
  ) %>%
  dplyr::group_by(geo_dist_cut, time_dist_cut) %>%
  dplyr::summarise(
    n = dplyr::n(),
    C1 = mean(C1_dist^2, na.rm = T),
    C1_resid = mean(C1_dist_resid^2, na.rm = T),
    C2 = mean(C2_dist^2, na.rm = T),
    C2_resid = mean(C2_dist_resid^2, na.rm = T),
    .groups	= "drop"
  )

d_binned_long <- d_binned %>%
  tidyr::pivot_longer(
    cols = tidyselect::one_of(c("C1", "C2", "C1_resid", "C2_resid")),
    names_to = "distance_type", values_to = "distance_value"
  ) %>%
  dplyr::mutate(
    detrended = ifelse(grepl("resid", distance_type), "detrended (residuals)", "not detrended"),
    distance_type = sub("_resid", "", distance_type),
    distance_type = factor(distance_type, levels = c("C1", "C2"))
  ) %>%
  dplyr::mutate(
    detrended = factor(detrended, levels = c("not detrended", "detrended (residuals)"))
  )

# plot loop
ps <- lapply(d_binned_long %>% dplyr::group_split(detrended, distance_type), function(x) {
  
  ggplot(x) + 
    geom_raster(
      aes(
        x = geo_dist_cut,
        y = time_dist_cut,
        fill = distance_value
      )
    ) +
    facet_grid(cols = dplyr::vars(detrended, distance_type)) +
    scale_fill_viridis_c(direction = -1) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 7, angle = 45, hjust = 0.9)
    ) +
    guides(
      fill = guide_colorbar(title = "genetic distance:", barwidth = 6)
    ) +
    xlab("spatial distance: 100km bins") +
    ylab("temporal distance: 100y bins")

})
  
p <- cowplot::plot_grid(plotlist = ps, nrow = 2, ncol = 2)

ggsave(
  "plots/figure_sup_1_variogram.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.4,
  dpi = 300,
  width = 500, height = 600, units = "mm",
  limitsize = F
)
