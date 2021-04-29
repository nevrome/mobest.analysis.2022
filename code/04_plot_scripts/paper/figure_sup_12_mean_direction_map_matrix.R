library(magrittr)
library(ggplot2)

#### data ####

# curves
load("data/poseidon_data/janno_final.RData")
load("data/origin_search/origin_grid_modified.RData")

# maps
load("data/spatial/mobility_regions.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")

#### map series ####

ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

mobility_maps <- origin_grid_modified %>% 
  dplyr::select(region_id, search_z_cut, angle_deg_cut, spatial_distance) %>%
  dplyr::filter(search_z_cut %in% seq(-6000, 1000, 1000)) %>%
  tidyr::complete(region_id, search_z_cut) %>%
  dplyr::group_by(region_id, search_z_cut, angle_deg_cut) %>%
  dplyr::summarise(mean_distance = mean(spatial_distance, na.rm = T), .groups = "drop") %>%
  dplyr::left_join(
    mobility_regions,
    by = "region_id"
  ) %>% sf::st_as_sf() %>%
  dplyr::mutate(
    z_named = dplyr::recode_factor(as.character(search_z_cut), !!!list(
      "-6000" = "6250-5750 calBC", 
      "-5000" = "5250-4750 calBC",
      "-4000" = "4250-3750 calBC",
      "-3000" = "3250-2750 calBC",
      "-2000" = "2250-1750 calBC",
      "-1000" = "1250-750 calBC",
      "0" = "250 calBC - 250 calAD",
      "1000"  = "750-1250 calAD"
    ))
  )
  
x_offset <- 180000
y_offset <- 180000

mobility_maps_center <- mobility_maps %>%
  sf::st_centroid() %>%
  dplyr::mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2],
    x = dplyr::case_when(
      angle_deg_cut == "E" ~ x + x_offset,
      angle_deg_cut == "W" ~ x - x_offset,
      TRUE ~ x
    ),
    y = dplyr::case_when(
      angle_deg_cut == "N" ~ y + y_offset,
      angle_deg_cut == "S" ~ y - y_offset,
      TRUE ~ y
    )
  )

p_map <- ggplot() +
  geom_sf(
    data = extended_area,
    fill = "white", colour = "darkgrey", size = 0.4
  ) +
  geom_point(
    data = mobility_maps_center,
    mapping = aes(
      x = x, 
      y = y,
      size = mean_distance,
      color = angle_deg_cut
    )
  ) +
  scale_size_continuous(
    range = c(0.1, 5), 
    name = "mean spatial distance [km]",
    breaks = round(diff(range(mobility_maps_center$mean_distance, na.rm = T))/5, -2)*(1:5)
  ) +
  facet_wrap(~z_named) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "#BFD5E3")
  ) +
  coord_sf(
    expand = FALSE,
    crs = epsg3035
  ) +
  scale_color_manual(
    values = c(
      "N" = "#F5793A", 
      "E" = "#85C0F9", 
      "S" = "#A95AA1",
      "W" = "#33a02c"
    ),
    guide = F
  )

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
    axis.text.x = element_text(size = 10, colour = "black")
  )
  

#### compile plots ####

p <- cowplot::ggdraw(p_map) +
  cowplot::draw_plot(
    p_legend,
    x = 0.7, y = 0.1, 
    width = 0.25, height = 0.25
  )


ggsave(
  paste0("plots/figure_sup_12_mean_direction_map_matrix.png"),
  plot = p,
  device = "png",
  scale = 0.5,
  dpi = 300,
  width = 360, height = 300, units = "mm",
  limitsize = F
)

