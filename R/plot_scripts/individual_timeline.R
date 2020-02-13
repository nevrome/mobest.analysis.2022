library(ggplot2)

load("data/gpr/pred_grid_spatial_cropped.RData")
load("data/spatial/extended_area.RData")
load("data/anno_1240K_and_anno_1240K_HumanOrigins_pca.RData")
ref_pops <- readLines("data/population_lists/PCA_6.pops")

# pca reference table
pca_ref <- anno_1240K_and_anno_1240K_HumanOrigins_pca %>%
  dplyr::filter(
    group_label %in% ref_pops
  )

# dots on the map
poi <- tibble::tribble(
  ~lat, ~lon,
  48.5, 10.5,
  44.2, 22.7,
  35.5, 36.9
) %>% sf::st_as_sf(
  coords = c("lon", "lat"),
  crs = 4326
) %>%
  sf::st_transform("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")

toi <- lapply(
  1:nrow(poi), function(i) {
    dm <- sf::st_distance(pred_grid_spatial_cropped, poi[i,])
    pred_grid_spatial_cropped[which(min(dm) == dm),]
  }
)

toi_dots <- lapply(
  toi, function(t) {
    tibble::tibble(
      x = t$x_real[1],
      y = t$y_real[1]
    )
  }
) %>% dplyr::bind_rows()


# plot map 
plot_map <- ggplot() +
  geom_sf(
    data = extended_area,
    fill = "white", colour = "black", size = 0.4
  ) +
  geom_point(
    data = toi_dots,
    aes(x = x, y = y),
    color = "red"
  )

# plot 
plots_pca <- lapply(toi, function(t) {
  ggplot() +
    geom_point(
      data = pca_ref,
      aes(x = PC1, y = PC2)
    ) +
    geom_path(
      data = t,
      aes(x = pred_PC1_mean, y = pred_PC2_mean)
    ) +
    geom_errorbar(
      data = t,
      aes(
        x = pred_PC1_mean, 
        ymin = pred_PC2_mean - pred_PC2_sd, ymax = pred_PC2_mean + pred_PC2_sd,
        color = age_sample
      ),
      alpha = 0.5
    ) +
    geom_errorbarh(
      data = t,
      aes(
        y = pred_PC2_mean, 
        xmin = pred_PC1_mean - pred_PC1_sd, xmax = pred_PC1_mean + pred_PC1_sd,
        color = age_sample
      ),
      alpha = 0.5
    ) +
    geom_point(
      data = t,
      aes(
        x = pred_PC1_mean, 
        y = pred_PC2_mean,
        color = age_sample
      ),
      size = 3
    ) +
    scale_color_gradient2(
      low = "red", mid = "green", high = "blue", midpoint = -6000
    )
})

# combine plots

cowplot::plot_grid()
