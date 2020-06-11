load("data/parameter_exploration/variogram/binned_data.RData")

p <- ggplot(d_binned) + 
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
  ylab("mean squared euclidean distance in PC1 & PC2 PCA space")

ggsave(
  "plots/figure_sup_1_variogram.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 200, units = "mm",
  limitsize = F
)
