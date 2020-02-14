library(magrittr)
library(ggplot2)

load("data/gpr/pred_grid_spatial_cropped.RData")
load("data/spatial/research_area.RData")
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
  43, -7.5,
  48.5, 10.5,
  44.2, 22.7,
  35.5, 36.9,
  43.9, 43.9
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

names(toi) <- letters[1:length(toi)]
toi_dots$id <- letters[1:length(toi)]

# plot map 
ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

plot_map <- ggplot() +
  geom_sf(
    data = extended_area,
    fill = "white", colour = "black", size = 0.4
  ) +
  geom_point(
    data = toi_dots,
    aes(x = x, y = y),
    color = "red"
  ) +
  ggrepel::geom_text_repel(
    data = toi_dots,
    aes(x = x, y = y, label = id),
    color = "red",
    size = 5
  ) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_line(colour = "grey", size = 0.3),
  ) +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = sf::st_crs("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
  )

# plot 
plots_pca <- lapply(toi, function(t) {
  ggplot() +
    geom_point(
      data = pca_ref,
      aes(x = PC1, y = PC2),
      color = "grey"
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
      limits = c(-7000, -500), low = "red", mid = "green", high = "blue", midpoint = -3000
    ) +
    theme_bw()
})

# combine plots

top_row_right_column <- cowplot::plot_grid(plotlist = plots_pca[1:2], ncol = 1, labels = c("a", "b"))

top_row <- cowplot::plot_grid(plot_map, top_row_right_column, ncol = 2, rel_widths = c(1, 0.5))

bottom_row <- cowplot::plot_grid(plotlist = plots_pca[3:5], ncol = 3, labels = c("c", "d", "e"))

p <- cowplot::plot_grid(top_row, bottom_row, nrow = 2, rel_heights = c(1, 0.5))

ggsave(
  "plots/individual_timeline.jpeg",
  plot = p,
  device = "jpeg",
  scale = 1,
  dpi = 300,
  width = 550, height = 280, units = "mm",
  limitsize = F
)
