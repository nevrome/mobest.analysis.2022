library(magrittr)
library(ggplot2)

#### plot resulting model ####

plotfun <- function(pdi, iti, ksi, dvi, color = "viridis") {
  plot <- ggplot() +
    geom_sf(data = extended_area, fill = "white") +
    geom_raster(
      data = pdi,
      aes(x = x_real, y = y_real, fill = mean, alpha = sd)
    ) +
    geom_sf(
      data = anno_slices_geo %>% dplyr::mutate(age_sample = age),
      mapping = aes_string(fill = dvi),
      size = 3.5,
      shape = 21,
      color = "black"
    ) +
    geom_sf(
      data = research_area,
      fill = NA, colour = "red", size = 0.4
    ) +
    scale_fill_viridis_c(
      limits = anno_slices_geo[[dvi]] %>% range,
      na.value = NA,
      oob = scales::squish,
      option = color,
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
      paste0("plots/gpr_maps/gpr_map_", paste(c(dvi, ksi, iti), collapse = "_"), ".jpeg"),
      plot = .,
      device = "jpeg",
      scale = 1,
      dpi = 300,
      width = 550, height = 260, units = "mm",
      limitsize = F
    )
}

load("data/gpr/pred_grid_filled_grouped_spatial.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/anno_slices_geo.RData")
ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

plot_grid <- pred_grid_filled_grouped_spatial %>% 
  tibble::as_tibble() %>%
  dplyr::select(dependent_var_id, kernel_setting_id, independent_table_type) %>%
  unique

pred_data <- pred_grid_filled_grouped_spatial %>%
  dplyr::filter(
    age_sample %% 500 == 0
  )

lapply(1:nrow(plot_grid), function(i) {
  iti <- plot_grid$independent_table_type[i]
  ksi <- plot_grid$kernel_setting_id[i] 
  dvi <- plot_grid$dependent_var_id[i]
  pdi <- pred_data %>% dplyr::filter(
    independent_table_type == plot_grid$independent_table_type[i], 
    kernel_setting_id == plot_grid$kernel_setting_id[i],
    dependent_var_id == plot_grid$dependent_var_id[i]
  )
  plotfun(pdi, iti, ksi, dvi)
})

