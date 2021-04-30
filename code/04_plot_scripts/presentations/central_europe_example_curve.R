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

# filter for central europe

janno_final %<>% dplyr::filter(region_id == "Central Europe")
origin_grid_median_modified %<>% dplyr::filter(region_id == "Central Europe")
moving_origin_grid %<>% dplyr::filter(region_id == "Central Europe")
mean_origin %<>% dplyr::filter(region_id == "Central Europe")
no_data_windows %<>% dplyr::filter(region_id == "Central Europe")

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

add_legend <- function(p) {
  cowplot::ggdraw(p) +
  cowplot::draw_plot(
    p_legend, .70, .60, .35, .35
  )
}

render_plot <- function(p, path) {
  ggsave(
    path,
    plot = p %>% add_legend(),
    device = "png",
    scale = 0.35,
    dpi = 500,
    width = 850, height = 400, units = "mm",
    limitsize = F
  )
}

#### prepare and render sequence of plots ####

p0 <- ggplot() +
  theme_bw() +
  theme(
    legend.position = "bottom",
  ) +
  xlab("time in years calBC/calAD") +
  ylab("spatial distance to \"link point\" (undirected mean) [km]") +
  scale_color_gradientn(
    colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"),
    na.value = NA,
    guide = F
  ) +
  scale_x_continuous(breaks = seq(-7000, 1000, 1000)) +
  coord_cartesian(
    xlim = c(-7000, 1000),
    ylim = c(-30, max(origin_grid_median_modified$spatial_distance, na.rm = T))
  )

p1 <- p0 + 
  geom_point(
    data = origin_grid_median_modified,
    mapping = aes(
      x = search_z, y = spatial_distance, color = angle_deg
    ),
    alpha = 1,
    size = 3,
    shape = 4
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
    data = janno_final %>% dplyr::filter(!is.na(region_id)),
    aes(x = Date_BC_AD_Median_Derived, y = -100),
    shape = "|",
    size = 3
  )

render_plot(p1, "plots/presentation/central_europe_example_p1.png")

p2 <- p0 + 
  geom_point(
    data = origin_grid_median_modified,
    mapping = aes(
      x = search_z, y = spatial_distance, color = angle_deg
    ),
    alpha = 1,
    size = 3,
    shape = 4
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
    data = janno_final %>% dplyr::filter(!is.na(region_id)),
    aes(x = Date_BC_AD_Median_Derived, y = -100),
    shape = "|",
    size = 3
  ) + ggrepel::geom_label_repel(
    data = origin_grid_median_modified %>% dplyr::filter(
      search_id %in% c("Stuttgart_published.DG", "RISE434.SG")
    ),
    mapping = aes(
      x = search_z, y = spatial_distance,
      label = search_id
    ),
    size = 4,
    ylim = c(3000, 4000),
    xlim = c(-7000, -4500),
    direction = "x",
    arrow = arrow(length = unit(0.015, "npc"), type = "closed"),
    #segment.curvature = 0.2,
    point.padding = 2
  )

render_plot(p2, "plots/presentation/central_europe_example_p2.png")

p3 <- p1 + 
  geom_line(
    data = moving_origin_grid,
    mapping = aes(x = z, y = undirected_mean_spatial_distance),
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
    data = janno_final %>% dplyr::filter(!is.na(region_id)),
    aes(x = Date_BC_AD_Median_Derived, y = -100),
    shape = "|",
    size = 3
  )

render_plot(p3, "plots/presentation/central_europe_example_p3.png")

p4 <- p0 + 
  geom_rect(
    data = no_data_windows,
    mapping = aes(
      ymax = Inf,
      ymin = -Inf,
      xmin = min_date_not_covered,
      xmax = max_date_not_covered
    ),
    fill = "lightgrey"
  ) +
  geom_ribbon(
    data = moving_origin_grid,
    mapping = aes(
      x = z,
      ymin = undirected_mean_spatial_distance - 2*sd_spatial_distance,
      ymax = undirected_mean_spatial_distance + 2*sd_spatial_distance
    ),
    fill = "lightgrey",
    alpha = 0.3
  ) +
  geom_ribbon(
    data = moving_origin_grid,
    mapping = aes(
      x = z,
      ymin = undirected_mean_spatial_distance - 2*std_spatial_distance,
      ymax = undirected_mean_spatial_distance + 2*std_spatial_distance
    ),
    fill = "lightgrey",
  ) +
  geom_line(
    data = moving_origin_grid,
    mapping = aes(x = z, y = undirected_mean_spatial_distance),
    size = 0.4
  ) + 
  geom_point(
    data = origin_grid_median_modified,
    mapping = aes(
      x = search_z, y = spatial_distance, color = angle_deg
    ),
    alpha = 1,
    size = 3,
    shape = 4
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
    data = janno_final %>% dplyr::filter(!is.na(region_id)),
    aes(x = Date_BC_AD_Median_Derived, y = -100),
    shape = "|",
    size = 3
  )
  
render_plot(p4, "plots/presentation/central_europe_example_p4.png")
