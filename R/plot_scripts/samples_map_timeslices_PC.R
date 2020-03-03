library(magrittr)
library(ggplot2)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/anno_slices_geo.RData")

ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

plotfun <- function(PC, color) {
  plot <- ggplot() +
    geom_sf(
      data = extended_area,
      fill = "white", colour = "black", size = 0.4
    ) +
    geom_sf(
      data = research_area,
      fill = NA, colour = "red", size = 0.4
    ) +
    geom_sf(
      data = anno_slices_geo,
      mapping = aes_string(
        color = PC,
        alpha = "norm_dens"
      ),
      size = 3.5,
      show.legend = "point"
    ) +
    theme_bw() +
    coord_sf(
      xlim = xlimit, ylim = ylimit,
      crs = sf::st_crs("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
    ) + 
    scale_color_viridis_c(option = color) +
    theme(
      plot.title = element_text(size = 30, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 20, face = "bold"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.text = element_text(size = 20),
      panel.grid.major = element_line(colour = "grey", size = 0.3),
      strip.text.x = element_text(size = 20)
    ) +
    guides(
      color = guide_colorbar(barwidth = 25),
      alpha = FALSE
    ) +
    facet_wrap(
      nrow = 3,
      ~age_slice
    )

  ggsave(
    paste0("plots/map_timeslices_", PC, ".jpeg"),
    plot = plot,
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

