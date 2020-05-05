library(magrittr)
library(ggplot2)

load("data/gpr/interpol_grid_spatial.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/anno_slices_geo.RData")
ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

p_PC1 <- interpol_grid_spatial %>%
  dplyr::filter(
    independent_table_id == "age_center",
    dependent_var_id %in% c("PC1"),
    kernel_setting_id == "ds200_dt400_g01",
    pred_grid_id == "scs100_tl100",
    z %in% c(-7000, -5000, -3000, -1000)
  ) %>%
  ggplot() +
  geom_sf(data = extended_area, fill = "white") +
  geom_raster(aes(x, y, fill = mean)) +
  facet_grid(rows = dplyr::vars(z), cols = dplyr::vars(dependent_var_id)) +
  geom_sf(data = extended_area, fill = NA) +
  geom_sf(
    data = anno_slices_geo %>% dplyr::mutate(z = age) %>% dplyr::filter(z %in% c(-7000, -5000, -3000, -1000)),
    aes(shape = age_group_id),
    size = 2,
    color = "white"
  ) +
  scale_shape_manual(
    values = c(
      ">-8000" = 15,
      "-8000 - -6000" = 15,
      "-6000 - -4000" = 17,
      "-4000 - -2000" = 6,
      "-2000 - 0" = 4
    ),
    guide = FALSE
  ) +
  scale_fill_viridis_c() +
  theme_bw() +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = sf::st_crs("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
  ) +
  guides(
    fill = guide_colorbar(title = "PC prediction", barwidth = 10)
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 17),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(size = 17),
    strip.text = element_text(size = 17),
    strip.background = element_rect(fill = NA),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#BFD5E3")
  )

p_PC2 <- interpol_grid_spatial %>%
  dplyr::filter(
    independent_table_id == "age_center",
    dependent_var_id %in% c("PC2"),
    kernel_setting_id == "ds200_dt400_g01",
    pred_grid_id == "scs100_tl100",
    z %in% c(-7000, -5000, -3000, -1000)
  ) %>%
  ggplot() +
  geom_sf(data = extended_area, fill = "white") +
  geom_raster(aes(x, y, fill = mean)) +
  facet_grid(rows = dplyr::vars(z), cols = dplyr::vars(dependent_var_id)) +
  geom_sf(data = extended_area, fill = NA) +
  geom_sf(
    data = anno_slices_geo %>% dplyr::mutate(z = age) %>% dplyr::filter(z %in% c(-7000, -5000, -3000, -1000)),
    aes(shape = age_group_id),
    size = 2,
    color = "white"
  ) +
  scale_shape_manual(
    values = c(
      ">-8000" = 15,
      "-8000 - -6000" = 15,
      "-6000 - -4000" = 17,
      "-4000 - -2000" = 6,
      "-2000 - 0" = 4
    ),
    guide = FALSE
  ) +
  scale_fill_viridis_c(option = "magma") +
  theme_bw() +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = sf::st_crs("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
  ) +
  guides(
    fill = guide_colorbar(title = "PC prediction", barwidth = 10)
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 17),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(size = 17),
    strip.text = element_text(size = 17),
    strip.background = element_rect(fill = NA),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#BFD5E3")
  )
  

# merge plots
p <- cowplot::plot_grid(p_PC1, p_PC2, ncol = 2)

ggsave(
  "plots/figure_3_interpolation_map_matrix.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.8,
  dpi = 300,
  width = 300, height = 350, units = "mm",
  limitsize = F
)

