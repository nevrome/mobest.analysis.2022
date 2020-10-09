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
      breaks = seq(-8000, 2000, 2000), 
      labels = seq(-7000, 2000, 2000),
      include.lowest = T
    )))
  )

ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

p_C1 <- interpol_grid %>%
  dplyr::filter(
    independent_table_id == "age_sample_1",
    dependent_var_id %in% "C1",
    kernel_setting_id == "ds800_dt1400_g001",
    pred_grid_id == "scs100_tl100",
    z %in% seq(-7000, 1000, 2000)
  ) %>%
  ggplot() +
  geom_sf(data = extended_area, fill = "white") +
  geom_raster(aes(x, y, fill = mean)) +
  facet_grid(rows = dplyr::vars(z), cols = dplyr::vars(dependent_var_id)) +
  geom_sf(data = extended_area, fill = NA, colour = "black") +
  geom_point(
    data = . %>% dplyr::filter(sd > (0.2 * diff(range(mean)))),
    aes(x, y), alpha = 0.8, color = "grey", shape = 4
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
    fill = guide_colorbar(title = "Prediction", barwidth = 10)
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 17),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 17),
    strip.background = element_rect(fill = NA),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#BFD5E3")
  )

p_C2 <- interpol_grid %>%
  dplyr::filter(
    independent_table_id == "age_sample_1",
    dependent_var_id %in% "C2",
    kernel_setting_id == "ds800_dt1400_g001",
    pred_grid_id == "scs100_tl100",
    z %in% seq(-7000, 1000, 2000)
  ) %>%
  ggplot() +
  geom_sf(data = extended_area, fill = "white") +
  geom_raster(aes(x, y, fill = mean)) +
  facet_grid(rows = dplyr::vars(z), cols = dplyr::vars(dependent_var_id)) +
  geom_sf(data = extended_area, fill = NA, colour = "black") +
  geom_point(
    data = . %>% dplyr::filter(sd > (0.2 * diff(range(mean)))),
    aes(x, y), alpha = 0.8, color = "grey", shape = 4
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
  scale_fill_viridis_c(option = "magma") +
  theme_bw() +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = epsg102013
  ) +
  guides(
    fill = guide_colorbar(title = "Prediction", barwidth = 10)
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 17),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 17),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#BFD5E3")
  )
  

# merge plots
p <- cowplot::plot_grid(p_C1, p_C2, ncol = 2)

ggsave(
  "plots/figure_3_interpolation_map_matrix.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.8,
  dpi = 300,
  width = 300, height = 420, units = "mm",
  limitsize = F
)

