library(magrittr)
library(ggplot2)

#### data ####

# curves
load("data/poseidon_data/janno_final.RData")
load("data/origin_search/origin_grid_median_modified.RData")
load("data/origin_search/moving_origin_grid.RData")
load("data/origin_search/mean_origin.RData")
load("data/origin_search/no_data_windows.RData")

# maps
load("data/spatial/mobility_regions.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
load("data/plot_reference_data/region_id_shapes.RData")

#### mobility estimator curves ####

p_estimator <- ggplot() +
  geom_vline(
    data = data.frame(x = c(-5500, -2700, 100)),
    aes(xintercept = x),
    linetype = "dotted"
  ) +
  facet_wrap(dplyr::vars(region_id)) +
  geom_point(
    data = origin_grid_median_modified,
    mapping = aes(
      x = search_z, y = spatial_distance, color = angle_deg,
      shape = origin_region_id
    ),
    alpha = 0.6,
    size = 0.6
  ) +
  scale_shape_manual(
    values = region_id_shapes,
    guide = F,
    na.value = 4
  ) +
  geom_rect(
    data = no_data_windows,
    mapping = aes(
      ymax = Inf,
      ymin = -Inf,
      xmin = min_date_not_covered,
      xmax = max_date_not_covered
    ),
    fill = "lightgrey",
    alpha = 0.7
  ) +
  geom_ribbon(
    data = moving_origin_grid,
    mapping = aes(
      x = z,
      ymin = undirected_mean_spatial_distance - 2*std_spatial_distance,
      ymax = undirected_mean_spatial_distance + 2*std_spatial_distance
    ),
    fill = "lightgrey",
    alpha = 0.7
  ) +
  geom_line(
    data = moving_origin_grid,
    mapping = aes(x = z, y = undirected_mean_spatial_distance, color = mean_angle_deg),
    size = 0.4
  ) +
  geom_rect(
    data = tibble::tibble(xmin = -Inf, ymin = -Inf, ymax = 0, xmax = Inf),
    mapping = aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax
    ),
    fill = "white"
  ) +
  geom_point(
    data = janno_final,
    aes(x = Date_BC_AD_Median_Derived, y = -100),
    shape = "|"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 40, hjust = 1)
  ) +
  xlab("time in years calBC/calAD") +
  ylab("spatial distance to \"origin\" (undirected mean) [km]") +
  scale_color_gradientn(
    colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"),
    na.value = NA,
    guide = F
  ) +
  scale_x_continuous(breaks = seq(-7000, 1000, 1000)) +
  coord_cartesian(
    #ylim = c(-100, 2000)
    #ylim = c(0, max(origin_grid$spatial_distance, na.rm = T))
  )

#### map series ####

ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

mobility_maps <- mean_origin %>%
  dplyr::filter(search_z_cut %in% c(-5500, -2700, 100)) %>%
  tidyr::complete(region_id, search_z_cut) %>%
  dplyr::mutate(
    mean_angle_deg_text = 360 - mean_angle_deg,
    z_named = dplyr::recode_factor(as.character(search_z_cut), !!!list(
      "-5500" = "5600-5400 calBC", 
      "-2700" = "2800-2600 calBC", 
      "100" = "0 calBC/AD - 200 calAD"
    ))
  ) %>%
  dplyr::left_join(
    mobility_regions,
    by = "region_id"
  ) %>% sf::st_as_sf()

mobility_maps_center <- mobility_maps %>%
  sf::st_centroid() %>%
  dplyr::mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]
  )

p_map <- ggplot() +
  geom_sf(
    data = extended_area,
    fill = "white", colour = "black", size = 0.4
  ) +
  geom_sf(
    data = mobility_maps,
    fill = "white", 
    alpha = 0.7,
    color = "black",
    size = 0.4
  ) +
  geom_text(
    data = mobility_maps_center %>% dplyr::filter(!is.na(directed_mean_spatial_distance)),
    mapping = aes(
      x = x, y = y, 
      color = mean_angle_deg, 
      angle = mean_angle_deg_text,
      size = directed_mean_spatial_distance
    ),
    label="\u2191"
  ) +
  scale_size_continuous(
    range = c(3, 12), name = "spatial distance to \"origin\"\n(directed mean) [km]",
    breaks = round(diff(range(mobility_maps_center$directed_mean_spatial_distance, na.rm = T))/5, -2)*(1:5),
    guide = guide_legend(nrow = 1, label.position = "bottom")
  ) +
  scale_color_gradientn(
    colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"),
    guide = F
  ) +
  facet_grid(cols = dplyr::vars(z_named)) +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "#BFD5E3")
  ) +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = epsg3035
  )

p_arrows_legend <- cowplot::get_legend(p_map)
p_map <- p_map + theme(legend.position = "none")

#### direction legend ####

p_legend <- tibble::tibble(
  ID = letters[1:8],
  angle_start = seq(0, 325, 45),
  angle_stop = seq(45, 360, 45)
) %>%
  ggplot() + 
  geom_rect(
    aes(xmin = 3, xmax = 4, ymin = angle_start, ymax = angle_stop, fill = ID)
  ) +
  scale_fill_manual(
    values = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"), 
    guide = FALSE
  ) +
  coord_polar(theta = "y") +
  xlim(2, 4.5) +
  scale_y_continuous(
    breaks = c(0, 45, 90, 135, 180, 225, 270, 315),
    labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10)
  )
  

#### compile plots ####

p_double_legend <- cowplot::plot_grid(p_legend, p_arrows_legend, ncol = 2, rel_widths = c(0.6, 1))

plot_bottom <- cowplot::plot_grid(p_map, p_double_legend, ncol = 2, rel_widths = c(0.7, 0.3))

p <- cowplot::plot_grid(
  p_estimator, plot_bottom, nrow = 2, rel_heights = c(1, 0.44), labels = c("A", "B"),
  label_y = c(1, 1.1)
)

ggsave(
  paste0("plots/figure_5_mobility_curves-age_resampling+one_kernel_setting.png"),
  plot = p,
  device = "png",
  scale = 0.5,
  dpi = 300,
  width = 700, height = 400, units = "mm",
  limitsize = F
)

