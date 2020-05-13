library(magrittr)
library(ggplot2)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_pca.RData")
ref_pops <- readLines("data/population_lists/PCA_6.pops")

pca_ref <- anno_1240K_and_anno_1240K_HumanOrigins_pca %>%
  dplyr::filter(
    group_label %in% ref_pops
  )

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final

load("data/gpr/interpol_grid_spatial_Berlin.RData")
load("data/gpr/interpol_grid_Berlin.RData")

#### filter ####
poi_timeseries <- interpol_grid %>%
  dplyr::filter(
    z %% 500 == 0
  ) %>% 
  tidyr::pivot_wider(
    id_cols = c("z", "kernel_setting_id"),
    names_from = "dependent_var_id",
    values_from = c("mean", "sd")
  )

poi_timeseries_large_kernel <- poi_timeseries %>% dplyr::filter(
  kernel_setting_id == "ds1000_dt1000_g001"
)
poi_timeseries_medium_kernel <- poi_timeseries %>% dplyr::filter(
  kernel_setting_id == "ds500_dt500_g001"
)
poi_timeseries_small_kernel <- poi_timeseries %>% dplyr::filter(
 kernel_setting_id == "ds200_dt200_g001"
)

#### pca ####

# large kernel
p_pca_large_kernel <- ggplot() +
  geom_point(
    data = pca_ref,
    aes(x = PC1, y = PC2),
    shape = 20,
    size = 0.2
  ) +
  geom_point(
    data = anno,
    aes(x = PC1, y = PC2, color = region_id, shape = age_group_id),
    alpha = 0.3,
    size = 2
  )  +
  scale_color_manual(
    values = c(
      "Central Europe" = "#999999", 
      "Iberia" = "#E69F00", 
      "Eastern Europe" = "#56B4E9", 
      "Britain and Ireland" = "#009E73", 
      "Turkey" = "#871200",
      "France" = "#F0E442", 
      "Near East" = "#0072B2", 
      "Caucasus" = "#D55E00", 
      "Italy" = "#CC79A7", 
      "Southeastern Europe" = "#2fff00"
    )
  ) +
  scale_shape_manual(
    values = c(
      ">-8000" = 15,
      "-8000 - -6000" = 15,
      "-6000 - -4000" = 17,
      "-4000 - -2000" = 6,
      "-2000 - 0" = 4
    )
  ) +
  ggnewscale::new_scale_color() +
  geom_path(
    data = poi_timeseries_large_kernel,
    aes(x = mean_PC1, y = mean_PC2),
    size = 1
  ) +
  geom_errorbar(
    data = poi_timeseries_large_kernel,
    aes(
      x = mean_PC1, 
      ymin = mean_PC2 - sd_PC2, ymax = mean_PC2 + sd_PC2,
      color = z
    ),
    alpha = 0.5
  ) +
  geom_errorbarh(
    data = poi_timeseries_large_kernel,
    aes(
      y = mean_PC2, 
      xmin = mean_PC1 - sd_PC1, xmax = mean_PC1 + sd_PC1,
      color = z
    ),
    alpha = 0.5
  ) +
  geom_point(
    data = poi_timeseries_large_kernel,
    aes(
      x = mean_PC1, 
      y = mean_PC2,
      color = z
    ),
    size = 3
  ) +
  scale_color_gradient2(
    limits = c(-7500, -500), low = "black", mid = "red", high = "green", midpoint = -5000,
    breaks = seq(-7500, -500, 1000)
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  coord_fixed(xlim = c(-0.12, 0.09), ylim = c(-0.11, 0.08)) +
  ggtitle("PCA: large kernel")

# medium kernel
p_pca_medium_kernel <- ggplot() +
  geom_point(
    data = pca_ref,
    aes(x = PC1, y = PC2),
    shape = 20,
    size = 0.2
  ) +
  geom_point(
    data = anno,
    aes(x = PC1, y = PC2, color = region_id, shape = age_group_id),
    alpha = 0.3,
    size = 2,
  )  +
  scale_color_manual(
    values = c(
      "Central Europe" = "#999999", 
      "Iberia" = "#E69F00", 
      "Eastern Europe" = "#56B4E9", 
      "Britain and Ireland" = "#009E73", 
      "Turkey" = "#871200",
      "France" = "#F0E442", 
      "Near East" = "#0072B2", 
      "Caucasus" = "#D55E00", 
      "Italy" = "#CC79A7", 
      "Southeastern Europe" = "#2fff00"
    )
  ) +
  scale_shape_manual(
    values = c(
      ">-8000" = 15,
      "-8000 - -6000" = 15,
      "-6000 - -4000" = 17,
      "-4000 - -2000" = 6,
      "-2000 - 0" = 4
    )
  ) +
  ggnewscale::new_scale_color() +
  geom_path(
    data = poi_timeseries_medium_kernel,
    aes(x = mean_PC1, y = mean_PC2),
    size = 1
  ) +
  geom_errorbar(
    data = poi_timeseries_medium_kernel,
    aes(
      x = mean_PC1, 
      ymin = mean_PC2 - sd_PC2, ymax = mean_PC2 + sd_PC2,
      color = z
    ),
    alpha = 0.5
  ) +
  geom_errorbarh(
    data = poi_timeseries_medium_kernel,
    aes(
      y = mean_PC2, 
      xmin = mean_PC1 - sd_PC1, xmax = mean_PC1 + sd_PC1,
      color = z
    ),
    alpha = 0.5
  ) +
  geom_point(
    data = poi_timeseries_medium_kernel,
    aes(
      x = mean_PC1, 
      y = mean_PC2,
      color = z
    ),
    size = 3
  ) +
  scale_color_gradient2(
    limits = c(-7500, -500), low = "black", mid = "red", high = "green", midpoint = -5000,
    breaks = seq(-7500, -500, 1000)
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  coord_fixed(xlim = c(-0.12, 0.09), ylim = c(-0.11, 0.08)) +
  ggtitle("PCA: medium kernel")

# small kernel
p_pca_small_kernel <- ggplot() +
  geom_point(
    data = pca_ref,
    aes(x = PC1, y = PC2),
    shape = 20,
    size = 0.2
  ) +
  geom_point(
    data = anno,
    aes(x = PC1, y = PC2, color = region_id, shape = age_group_id),
    alpha = 0.3,
    size = 2
  )  +
  scale_color_manual(
    values = c(
      "Central Europe" = "#999999", 
      "Iberia" = "#E69F00", 
      "Eastern Europe" = "#56B4E9", 
      "Britain and Ireland" = "#009E73", 
      "Turkey" = "#871200",
      "France" = "#F0E442", 
      "Near East" = "#0072B2", 
      "Caucasus" = "#D55E00", 
      "Italy" = "#CC79A7", 
      "Southeastern Europe" = "#2fff00"
    )
  ) +
  scale_shape_manual(
    values = c(
      ">-8000" = 15,
      "-8000 - -6000" = 15,
      "-6000 - -4000" = 17,
      "-4000 - -2000" = 6,
      "-2000 - 0" = 4
    )
  ) +
  ggnewscale::new_scale_color() +
  geom_path(
    data = poi_timeseries_small_kernel,
    aes(x = mean_PC1, y = mean_PC2),
    size = 1
  ) +
  geom_errorbar(
    data = poi_timeseries_small_kernel,
    aes(
      x = mean_PC1, 
      ymin = mean_PC2 - sd_PC2, ymax = mean_PC2 + sd_PC2,
      color = z
    ),
    alpha = 0.5
  ) +
  geom_errorbarh(
    data = poi_timeseries_small_kernel,
    aes(
      y = mean_PC2, 
      xmin = mean_PC1 - sd_PC1, xmax = mean_PC1 + sd_PC1,
      color = z
    ),
    alpha = 0.5
  ) +
  geom_point(
    data = poi_timeseries_small_kernel,
    aes(
      x = mean_PC1, 
      y = mean_PC2,
      color = z
    ),
    size = 3
  ) +
  scale_color_gradient2(
    limits = c(-7500, -500), low = "black", mid = "red", high = "green", midpoint = -5000,
    breaks = seq(-7500, -500, 1000)
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  coord_fixed(xlim = c(-0.12, 0.09), ylim = c(-0.11, 0.08)) +
  ggtitle("PCA: small kernel")

#### MDS ####

# large kernel
p_mds_large_kernel <- ggplot() +
  geom_point(
    data = anno,
    aes(x = C1, y = C2, color = region_id, shape = age_group_id),
    alpha = 0.7,
    size = 2
  )  +
  scale_color_manual(
    values = c(
      "Central Europe" = "#999999", 
      "Iberia" = "#E69F00", 
      "Eastern Europe" = "#56B4E9", 
      "Britain and Ireland" = "#009E73", 
      "Turkey" = "#871200",
      "France" = "#F0E442", 
      "Near East" = "#0072B2", 
      "Caucasus" = "#D55E00", 
      "Italy" = "#CC79A7", 
      "Southeastern Europe" = "#2fff00"
    )
  ) +
  scale_shape_manual(
    values = c(
      ">-8000" = 15,
      "-8000 - -6000" = 15,
      "-6000 - -4000" = 17,
      "-4000 - -2000" = 6,
      "-2000 - 0" = 4
    )
  ) +
  guides(
    color = guide_legend()
  ) +
  ggnewscale::new_scale_color() +
  geom_path(
    data = poi_timeseries_large_kernel,
    aes(x = mean_C1, y = mean_C2),
    size = 1
  ) +
  geom_errorbar(
    data = poi_timeseries_large_kernel,
    aes(
      x = mean_C1, 
      ymin = mean_C2 - sd_C2, ymax = mean_C2 + sd_C2,
      color = z
    ),
    alpha = 0.5
  ) +
  geom_errorbarh(
    data = poi_timeseries_large_kernel,
    aes(
      y = mean_C2, 
      xmin = mean_C1 - sd_C1, xmax = mean_C1 + sd_C1,
      color = z
    ),
    alpha = 0.5
  ) +
  geom_point(
    data = poi_timeseries_large_kernel,
    aes(
      x = mean_C1, 
      y = mean_C2,
      color = z
    ),
    size = 3
  ) +
  scale_color_gradient2(
    limits = c(-7500, -500), low = "black", mid = "red", high = "green", midpoint = -5000,
    breaks = seq(-7500, -500, 1000)
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  coord_fixed(xlim = c(-0.09, 0.07), ylim = c(0.07, -0.06)) +
  scale_y_reverse() +
  ggtitle("MDS: large kernel")

# medium kernel
p_mds_medium_kernel <- ggplot() +
  geom_point(
    data = anno,
    aes(x = C1, y = C2, color = region_id, shape = age_group_id),
    alpha = 0.7,
    size = 2
  )  +
  scale_color_manual(
    values = c(
      "Central Europe" = "#999999", 
      "Iberia" = "#E69F00", 
      "Eastern Europe" = "#56B4E9", 
      "Britain and Ireland" = "#009E73", 
      "Turkey" = "#871200",
      "France" = "#F0E442", 
      "Near East" = "#0072B2", 
      "Caucasus" = "#D55E00", 
      "Italy" = "#CC79A7", 
      "Southeastern Europe" = "#2fff00"
    )
  ) +
  scale_shape_manual(
    values = c(
      ">-8000" = 15,
      "-8000 - -6000" = 15,
      "-6000 - -4000" = 17,
      "-4000 - -2000" = 6,
      "-2000 - 0" = 4
    )
  ) +
  guides(
    color = guide_legend()
  ) +
  ggnewscale::new_scale_color() +
  geom_path(
    data = poi_timeseries_medium_kernel,
    aes(x = mean_C1, y = mean_C2),
    size = 1
  ) +
  geom_errorbar(
    data = poi_timeseries_medium_kernel,
    aes(
      x = mean_C1, 
      ymin = mean_C2 - sd_C2, ymax = mean_C2 + sd_C2,
      color = z
    ),
    alpha = 0.5
  ) +
  geom_errorbarh(
    data = poi_timeseries_medium_kernel,
    aes(
      y = mean_C2, 
      xmin = mean_C1 - sd_C1, xmax = mean_C1 + sd_C1,
      color = z
    ),
    alpha = 0.5
  ) +
  geom_point(
    data = poi_timeseries_medium_kernel,
    aes(
      x = mean_C1, 
      y = mean_C2,
      color = z
    ),
    size = 3
  ) +
  scale_color_gradient2(
    limits = c(-7500, -500), low = "black", mid = "red", high = "green", midpoint = -5000,
    breaks = seq(-7500, -500, 1000)
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  coord_fixed(xlim = c(-0.09, 0.07), ylim = c(0.07, -0.06)) +
  scale_y_reverse() +
  ggtitle("MDS: medium kernel")

# small kernel
p_mds_small_kernel <- ggplot() +
  geom_point(
    data = anno,
    aes(x = C1, y = C2, color = region_id, shape = age_group_id),
    alpha = 0.7,
    size = 2
  )  +
  scale_color_manual(
    values = c(
      "Central Europe" = "#999999", 
      "Iberia" = "#E69F00", 
      "Eastern Europe" = "#56B4E9", 
      "Britain and Ireland" = "#009E73", 
      "Turkey" = "#871200",
      "France" = "#F0E442", 
      "Near East" = "#0072B2", 
      "Caucasus" = "#D55E00", 
      "Italy" = "#CC79A7", 
      "Southeastern Europe" = "#2fff00"
    ),
    guide = guide_legend(title = "Region", ncol = 2)
  ) +
  scale_shape_manual(
    values = c(
      ">-8000" = 15,
      "-8000 - -6000" = 15,
      "-6000 - -4000" = 17,
      "-4000 - -2000" = 6,
      "-2000 - 0" = 4
    )
  ) +
  ggnewscale::new_scale_color() +
  geom_path(
    data = poi_timeseries_small_kernel,
    aes(x = mean_C1, y = mean_C2),
    size = 1
  ) +
  geom_errorbar(
    data = poi_timeseries_small_kernel,
    aes(
      x = mean_C1, 
      ymin = mean_C2 - sd_C2, ymax = mean_C2 + sd_C2,
      color = z
    ),
    alpha = 0.5
  ) +
  geom_errorbarh(
    data = poi_timeseries_small_kernel,
    aes(
      y = mean_C2, 
      xmin = mean_C1 - sd_C1, xmax = mean_C1 + sd_C1,
      color = z
    ),
    alpha = 0.5
  ) +
  geom_point(
    data = poi_timeseries_small_kernel,
    aes(
      x = mean_C1, 
      y = mean_C2,
      color = z
    ),
    size = 3
  ) +
  scale_color_gradient2(
    limits = c(-7500, -500), low = "black", mid = "red", high = "green", midpoint = -5000,
    breaks = seq(-7500, -500, 1000)
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  coord_fixed(xlim = c(-0.09, 0.07), ylim = c(0.07, -0.06)) +
  scale_y_reverse() +
  guides(
    color = guide_legend(title = "Time prediction", ncol = 1),
    shape = guide_legend(title = "Time", ncol = 1)
  ) +
  ggtitle("MDS: small kernel")

#### merge plots ####

p_pca_bottom <- cowplot::plot_grid(
  p_pca_medium_kernel, 
  p_pca_small_kernel, 
  nrow = 1, 
  labels = c("B", "C"), label_size = 17
)
p_pca <- cowplot::plot_grid(
  p_pca_large_kernel, 
  p_pca_bottom, 
  ncol = 1, rel_heights = c(1, 0.7), 
  labels = c("A", NA), label_size = 17
)

p_mds_bottom <- cowplot::plot_grid(
  p_mds_medium_kernel, 
  p_mds_small_kernel + theme(legend.position = "none"), 
  nrow = 1, 
  labels = c("E", "F"), label_size = 17
)
p_mds <- cowplot::plot_grid(
  p_mds_large_kernel, 
  p_mds_bottom, 
  ncol = 1, rel_heights = c(1, 0.7), 
  labels = c("D", NA), label_size = 17
)

p <- cowplot::plot_grid(
  p_pca, 
  cowplot::get_legend(p_mds_small_kernel), 
  p_mds, 
  ncol = 1, rel_heights = c(1, 0.2, 1)
)

ggsave(
  paste0("plots/figure_4_timepillars_Berlin.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.7,
  dpi = 300,
  width = 300, height = 750, units = "mm",
  limitsize = F
)

