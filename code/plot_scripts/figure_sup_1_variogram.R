library(ggplot2)
library(magrittr)

load("data/parameter_exploration/variogram/all_distances.RData")

# binning
d_binned <- d_all %>%
  dplyr::mutate(
    geo_dist_cut = cut(geo_dist, breaks = c(seq(0, max(geo_dist), 100), max(geo_dist)), include.lowest	= T, labels = F) * 100,
    time_dist_cut = cut(time_dist, breaks = c(seq(0, max(time_dist), 100), max(time_dist)), include.lowest	= T, labels = F) * 100
  ) %>%
  dplyr::group_by(geo_dist_cut, time_dist_cut) %>%
  dplyr::summarise(
    n = dplyr::n(),
    PC1 = mean(PC1_dist^2, na.rm = T),
    PC1_resid = mean(PC1_dist_resid^2, na.rm = T),
    PC2 = mean(PC2_dist^2, na.rm = T),
    PC2_resid = mean(PC2_dist_resid^2, na.rm = T),
    C1= mean(C1_dist^2, na.rm = T),
    C1_resid = mean(C1_dist_resid^2, na.rm = T),
    C2 = mean(C2_dist^2, na.rm = T),
    C2_resid = mean(C2_dist_resid^2, na.rm = T)
  ) %>%
  dplyr::ungroup()

d_binned_long <- d_binned %>%
  tidyr::pivot_longer(cols = tidyselect::one_of(c("PC1", "PC2", "C1", "C2", "PC1_resid", "PC2_resid", "C1_resid", "C2_resid")), names_to = "distance_type", values_to = "distance_value") %>%
  dplyr::mutate(
    detrended = ifelse(grepl("resid", distance_type), "not detrended", "detrended (residuals)"),
    distance_type = sub("_resid", "", distance_type),
    distance_type = factor(distance_type, levels = c("PC1", "PC2", "C1", "C2"))
  ) 


ggplot(d_binned_long) + 
  geom_raster(
    aes(
      x = geo_dist_cut,
      y = time_dist_cut,
      fill = distance_value
    )
  ) +
  facet_grid(rows = dplyr::vars(detrended), cols = dplyr::vars(distance_type)) +
  scale_fill_viridis_c(direction = -1) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    fill = guide_colorbar(title = "mean squared distance along different ancestry components")
  ) +
  xlab("spatial distance: 100km bins") +
  ylab("temporal distance: 100y bins")

p_resid <- ggplot(d_binned) + 
  geom_line(
    aes(
      x = geo_dist_cut,
      y = mean_sq_pca_dist,
      group = time_dist_cut,
      col = as.character(time_dist_cut)
    )
  ) +
  theme_bw() +
  guides(
    color = guide_legend(title = "temporal distance: 500y bins")
  ) +
  xlab("spatial distance: 100km bins") +
  ylab("mean squared euclidean distance in residual PC1 & PC2 PCA space")

ggsave(
  "plots/figure_sup_1_variogram.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 200, units = "mm",
  limitsize = F
)
