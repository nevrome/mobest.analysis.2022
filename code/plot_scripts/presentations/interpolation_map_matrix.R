library(magrittr)
library(ggplot2)

load("data/gpr/interpol_grid.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/plot_reference_data/age_group_id_shapes.RData")
load("data/spatial/epsg102013.RData")
load("data/poseidon_data/janno_final.RData")

janno_final <- janno_final %>% 
  dplyr::mutate(
    z = as.numeric(as.character(cut(
      Date_BC_AD_Median_Derived, 
      breaks = seq(-7500, 1500, 1000), 
      labels = seq(-7000, 1000, 1000),
      include.lowest = T
    )))
  ) %>%
  dplyr::filter(
    z %in% seq(-7000, 0, 1000)
  )

ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

p <- interpol_grid %>%
  dplyr::filter(
    independent_table_id == "age_sample_2",
    dependent_var_id %in% "C1",
    kernel_setting_id == "ds800_dt1400_g001",
    pred_grid_id == "scs100_tl100",
    z %in% seq(-7000, 0, 1000)
  ) %>%
  ggplot() +
  geom_sf(data = extended_area, fill = "white") +
  geom_raster(aes(x, y, fill = mean)) +
  facet_wrap(dplyr::vars(z), nrow = 2) +
  geom_sf(data = extended_area, fill = NA, colour = "black") +
  geom_point(
    data = . %>% dplyr::filter(sd > (0.2 * diff(range(mean)))),
    aes(x, y), alpha = 0.8, color = "red", shape = 4
  ) +
  geom_point(
    data = janno_final,
    aes(x, y, shape = age_group_id),
    size = 1.5,
    color = "white"
  ) +
  scale_shape_manual(
    values = age_group_id_shapes,
    guide = FALSE
  ) +
  scale_fill_viridis_c() +
  theme_bw() +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = epsg102013
  ) +
  guides(
    fill = guide_colorbar(title = "Prediction", barwidth = 30)
  ) +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 20, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(size = 20),
    panel.grid.major = element_line(colour = "grey", size = 0.3),
    strip.text.x = element_text(size = 20),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#BFD5E3")
  )

p %>% 
  ggsave(
    "plots/interpolation_map_matrix.jpeg",
    plot = .,
    device = "jpeg",
    scale = 0.7,
    dpi = 300,
    width = 750, height = 280, units = "mm",
    limitsize = F
  )

