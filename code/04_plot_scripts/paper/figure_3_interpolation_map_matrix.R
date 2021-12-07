library(magrittr)
library(ggplot2)

load("data/gpr/interpol_grid_median_selected_timeslices.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
load("data/poseidon_data/janno_final.RData")

janno_final <- janno_final %>% 
  dplyr::mutate(
    z = as.numeric(as.character(cut(
      Date_BC_AD_Median_Derived, 
      breaks = seq(-8000, 2000, 2000), 
      labels = seq(-7000, 1000, 2000),
      include.lowest = T
    )))
  )

p_C1 <- interpol_grid %>%
  dplyr::filter(
    dependent_var_id %in% "C1"
  ) %>%
  ggplot() +
  geom_sf(data = extended_area, fill = "black") +
  geom_raster(aes(x, y, fill = mean)) +
  facet_grid(cols = dplyr::vars(z), rows = dplyr::vars(dependent_var_id)) +
  geom_sf(data = extended_area, fill = NA, colour = "black") +
  # geom_point(
  #   data = . %>% dplyr::filter(sd > (0.15 * diff(range(mean)))),
  #   aes(x, y), alpha = 0.8, color = "grey", shape = 4
  # ) +
  geom_point(
    data = janno_final,
    aes(x, y),
    size = 0.5,
    color = "white"
  ) +
  scale_fill_viridis_c(
    breaks = seq(-0.1, 0.1, 0.02)
  ) +
  theme_bw() +
  coord_sf(
    expand = FALSE,
    crs = epsg3035
  ) +
  guides(
    fill = guide_colorbar(title = "Prediction C1  ", barwidth = 25)
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
  )

p_C2 <- interpol_grid %>%
  dplyr::filter(
    dependent_var_id %in% "C2"
  ) %>%
  ggplot() +
  geom_sf(data = extended_area, fill = "black") +
  geom_raster(aes(x, y, fill = mean)) +
  facet_grid(cols = dplyr::vars(z), rows = dplyr::vars(dependent_var_id)) +
  geom_sf(data = extended_area, fill = NA, colour = "black") +
  # geom_point(
  #   data = . %>% dplyr::filter(sd > (0.15 * diff(range(mean)))),
  #   aes(x, y), alpha = 0.8, color = "grey", shape = 4
  # ) +
  geom_point(
    data = janno_final,
    aes(x, y),
    size = 0.5,
    color = "white"
  ) +
  scale_fill_viridis_c(
    breaks = seq(-0.1, 0.1, 0.02),
    option = "magma"
  ) +
  theme_bw() +
  coord_sf(
    expand = FALSE,
    crs = epsg3035
  ) +
  guides(
    fill = guide_colorbar(title = "Prediction C2  ", barwidth = 25)
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
  )
  

# merge plots
C1_legend <- cowplot::get_legend(p_C1)
p_C1 <- p_C1 + theme(legend.position = "none")
C2_legend <- cowplot::get_legend(p_C2)
p_C2 <- p_C2 + theme(legend.position = "none")

plots <- cowplot::plot_grid(p_C1, p_C2, nrow = 2)
legends <- cowplot::plot_grid(C1_legend, C2_legend, nrow = 1)

p <- cowplot::plot_grid(plots, legends, nrow = 2, rel_heights = c(1, 0.15))

ggsave(
  "plots/figure_3_interpolation_map_matrix.pdf",
  plot = p,
  device = "pdf",
  scale = 0.5,
  dpi = 300,
  width = 770, height = 300, units = "mm",
  limitsize = F,
  bg = "white"
)
