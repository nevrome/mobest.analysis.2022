library(ggplot2)

load("data/gpr/pred_grid_spatial_cropped.RData")
load("data/spatial/extended_area.RData")
load("data/anno_1240K_and_anno_1240K_HumanOrigins_pca.RData")

hu <- pred_grid_spatial_cropped %>% 
  split(
    f = list(.$x_real, .$y_real)
  )

spu <- hu[[700]]

ggplot() +
  geom_sf(
    data = extended_area,
    fill = "white", colour = "black", size = 0.4
  ) +
  geom_sf(
    data = spu,
    color = "red"
  )


ref_pops <- readLines("data/population_lists/PCA_6.pops")

pca_ref <- anno_1240K_and_anno_1240K_HumanOrigins_pca %>%
  dplyr::filter(
    group_label %in% ref_pops
  )



ggplot() +
  geom_point(
    data = pca_ref,
    aes(x = PC1, y = PC2)
  ) +
  geom_path(
    data = spu,
    aes(x = pred_PC1_mean, y = pred_PC2_mean)
  ) +
  geom_point(
    data = spu,
    aes(
      x = pred_PC1_mean, 
      y = pred_PC2_mean,
      color = age_sample
    ),
    size = 3
  ) +
  scale_color_gradient2(
    low = "red", mid = "green", high = "blue", midpoint = -6000
  ) +
  geom_errorbar(
    data = spu,
    aes(x = pred_PC1_mean, ymin = pred_PC2_mean - pred_PC2_s2, ymax = pred_PC2_mean + pred_PC2_s2)
  ) +
  geom_errorbarh(
    data = spu,
    aes(y = pred_PC2_mean, xmin = pred_PC1_mean - pred_PC1_s2, xmax = pred_PC1_mean + pred_PC1_s2)
  )


