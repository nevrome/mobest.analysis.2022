library(magrittr)
library(ggplot2)

load("data/origin_search/interpol_grid_selected_timeslices.RData")
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

p_func <- function(cur_dependent_var, vis_var, legend_label, fill_scale) {
  interpol_grid %>%
    dplyr::filter(dependent_var_id %in% cur_dependent_var) %>%
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
    geom_raster(aes(x, y, fill = .data[[vis_var]])) +
    geom_sf(data = extended_area, fill = NA, colour = "black") + 
    theme_bw() +
    fill_scale +
    coord_sf(
      expand = FALSE,
      crs = epsg3035
    ) +
    guides(
      fill = guide_colorbar(title = legend_label, barwidth = 13)
    ) +
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
    ) +
    if (vis_var == "sd") {
      geom_point(
        data = janno_final,
        aes(x, y),
        size = 0.5,
        color = "red"
      )
    } else {
      NULL
    }
}

p_C1_mean <- p_func(
  "C1_mds_u", "mean", "Prediction C1  ",
  scale_fill_viridis_c(breaks = seq(-0.1, 0.1, 0.05))
)
p_C2_mean <- p_func(
  "C2_mds_u", "mean", "Prediction C2  ",
  scale_fill_viridis_c(breaks = seq(-0.1, 0.1, 0.05), option = "magma")
)
p_C1_sd   <- p_func(
  "C1_mds_u", "sd", "Standard deviation C1  ",
  scale_fill_gradientn(breaks = seq(0, 0.1, 0.01),
  colours = c("#424242", "#cccccc", "white"))
)
# p_C2_sd   <- p_func(
#   "C2_mds_u", "sd", "Standard deviation C2  ",
#   scale_fill_gradientn(breaks = seq(0, 0.1, 0.01),
#   colours = c("#424242", "#cccccc", "white"))
# )

# merge plots
C1_mean_legend <- cowplot::get_legend(p_C1_mean)
p_C1_mean <- p_C1_mean + theme(legend.position = "none")
C2_mean_legend <- cowplot::get_legend(p_C2_mean)
p_C2_mean <- p_C2_mean + theme(legend.position = "none")

C1_sd_legend <- cowplot::get_legend(p_C1_sd)
p_C1_sd <- p_C1_sd + theme(legend.position = "none")
# C2_sd_legend <- cowplot::get_legend(p_C2_sd)
# p_C2_sd <- p_C2_sd + theme(legend.position = "none")

plots <- cowplot::plot_grid(p_C1_mean, p_C2_mean, p_C1_sd, nrow = 3)
legends <- cowplot::plot_grid(C1_mean_legend, C2_mean_legend, C1_sd_legend, nrow = 1)
p <- cowplot::plot_grid(plots, legends, nrow = 2, rel_heights = c(1, 0.1))

ggsave(
  "plots/figure_3_interpolation_map_matrix.pdf",
  plot = p,
  device = "pdf",
  scale = 0.5,
  dpi = 300,
  width = 770, height = 430, units = "mm",
  limitsize = F,
  bg = "white",
  compress = FALSE
)
