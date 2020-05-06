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
    dependent_var_id %in% c("PC1", "PC2"),
    z %% 500 == 0
  ) %>% 
  tidyr::pivot_wider(
    id_cols = c("z", "kernel_setting_id"),
    names_from = "dependent_var_id",
    values_from = c("mean", "sd")
  )

# large kernel
poi_timeseries_large_kernel <- poi_timeseries %>% dplyr::filter(kernel_setting_id == "ds1000_dt1000_g001")
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
    aes(x = mean_PC1, y = mean_PC2)
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
    legend.position = "right"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  coord_fixed()

# medium kernel
poi_timeseries_medium_kernel <- poi_timeseries %>% dplyr::filter(kernel_setting_id == "ds500_dt500_g001")
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
    aes(x = mean_PC1, y = mean_PC2)
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
  coord_fixed()

# small kernel
poi_timeseries_small_kernel <- poi_timeseries %>% dplyr::filter(kernel_setting_id == "ds200_dt200_g001")
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
    aes(x = mean_PC1, y = mean_PC2)
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
  coord_fixed()

p_pca_bottom <- cowplot::plot_grid(p_pca_medium_kernel, p_pca_small_kernel, nrow = 1)
p_pca <- cowplot::plot_grid(p_pca_large_kernel, p_pca_bottom, ncol = 1, rel_heights = c(1, 0.7))

