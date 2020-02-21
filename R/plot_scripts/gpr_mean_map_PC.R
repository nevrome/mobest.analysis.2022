library(magrittr)
library(ggplot2)

load("data/gpr/pred_grid_spatial_cropped.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/anno_slices_geo.RData")

ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

#### plot resulting model ####

plotfun <- function(PC, color) {
  plot <- ggplot() +
    geom_sf(data = extended_area, fill = "white") +
    geom_raster(
      data = pred_grid_spatial_cropped,
      aes_string(x = "x_real", y = "y_real", fill = paste0("pred_", PC, "_mean"), alpha = paste0("pred_", PC, "_s2"))
    ) +
    geom_sf(
      data = anno_slices_geo %>% dplyr::mutate(age_sample = age),
      mapping = aes_string(fill = PC),
      size = 3.5,
      shape = 21,
      color = "black"
    ) +
    geom_sf(
      data = research_area,
      fill = NA, colour = "red", size = 0.4
    ) +
    scale_fill_viridis_c(
      limits = pred_grid_spatial_cropped[[paste0("pred_", PC, "_mean")]]  %>% range,
      na.value = NA,
      oob = scales::squish,
      option = color
    ) +
    scale_alpha_continuous(range = c(1, 0), na.value = 0) +
    theme_bw() +
    coord_sf(
      xlim = xlimit, ylim = ylimit,
      crs = sf::st_crs("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
    ) +
    guides(
      fill = guide_colorbar(title = "PC prediction", barwidth = 25),
      alpha = guide_legend(title = "SD", override.aes = list(size = 10), nrow = 1, byrow = TRUE, order = 2)
    ) +
    theme(
      plot.title = element_text(size = 30, face = "bold"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = 20, face = "bold"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.text = element_text(size = 20),
      panel.grid.major = element_line(colour = "grey", size = 0.3),
      strip.text.x = element_text(size = 20)
    ) +
    facet_wrap(
      ~age_sample, 
      nrow = 3
    )
  
  plot %>%
    ggsave(
      paste0("plots/gpr_mean_map_", PC, ".jpeg"),
      plot = .,
      device = "jpeg",
      scale = 1,
      dpi = 300,
      width = 550, height = 260, units = "mm",
      limitsize = F
    )
}

plotfun("PC1", "viridis")
plotfun("PC2", "plasma")
plotfun("PC3", "cividis")
plotfun("PC4", "inferno")
