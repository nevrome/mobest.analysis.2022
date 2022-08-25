library(magrittr)
library(ggplot2)

load("data/gpr/interpol_grid_median_selected_timeslices.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
load("data/genotype_data/janno_final.RData")

janno_final <- janno_final %>% 
  dplyr::mutate(
    z = as.numeric(as.character(cut(
      Date_BC_AD_Median_Derived, 
      breaks = seq(-8000, 2000, 2000), 
      labels = seq(-7000, 1000, 2000),
      include.lowest = T
    )))
  )

p_map_matrix <- function(depvar, viridis_option, viridis_direction) {
  interpol_grid %>%
    dplyr::filter(dependent_var_id %in% depvar) %>%
    ggplot() +
    facet_grid(
      cols = dplyr::vars(z), rows = dplyr::vars(dependent_var_id),
      labeller = labeller(
        z = as_labeller(
          interpol_grid %>%
            dplyr::mutate(
              z_label = paste0(abs(z), " ", ifelse(z < 0, "BC", "AD"))
            ) %>%
            dplyr::select(z, z_label) %>%
            unique %>%
            tibble::deframe()
        )
      )
    ) +
    geom_sf(data = extended_area, fill = "black") +
    geom_raster(aes(x, y, fill = mean)) +
    geom_sf(data = extended_area, fill = NA, colour = "black") +
    # geom_point(
    #   data = . %>% dplyr::filter(sd > (0.15 * diff(range(mean)))),
    #   aes(x, y), alpha = 0.8, color = "grey", shape = 4
    # ) +
    # geom_point(
    #   data = janno_final,
    #   aes(x, y),
    #   size = 0.5,
    #   color = "white"
    # ) +
    scale_fill_viridis_c(option = viridis_option, direction = viridis_direction) +
    theme_bw() +
    coord_sf(expand = FALSE, crs = epsg3035) +
    guides(fill = guide_colorbar(title = paste0("Prediction for ", depvar ," "), barwidth = 25)) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = 12),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.text = element_text(size = 12),
      strip.text = element_text(size = 15),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "#BFD5E3")
    )
}

p_C1 <- p_map_matrix("C1_pca_proj_u", "viridis", 1)
p_C2 <- p_map_matrix("C2_pca_proj_u", "magma", -1)
p_C3 <- p_map_matrix("C3_pca_proj_u", "cividis", 1)
p_C4 <- p_map_matrix("C4_pca_proj_u", "inferno", -1)
p_C5 <- p_map_matrix("C5_pca_proj_u", "plasma", -1)

p <- cowplot::plot_grid(p_C1, p_C2, p_C3, p_C4, p_C5, nrow = 5, ncol = 1)

ggsave(
  "plots/figure_sup_18_interpolation_map_matrix_pca_proj.pdf",
  plot = p,
  device = "pdf",
  scale = 0.9,
  dpi = 300,
  width = 450, height = 480, units = "mm",
  limitsize = F,
  bg = "white"
)

