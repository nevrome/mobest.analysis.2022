library(magrittr)
library(ggplot2)

#### data ####

# curves
load("data/poseidon_data/janno_final.RData")
load("data/origin_search/origin_grid_mean.RData")
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

rois <- c("Eastern Balkan", "Britain and Ireland", "Italy, Sardinia, Adria")
janno_final %<>% dplyr::filter(region_id %in% rois)
origin_grid_mean %<>% dplyr::filter(region_id %in% rois)
moving_origin_grid %<>% dplyr::filter(region_id %in% rois)
mean_origin %<>% dplyr::filter(region_id %in% rois)
no_data_windows %<>% dplyr::filter(region_id %in% rois)

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

ggsave(
  filename = "plots/presentation/windrose_legend.png",
  plot = p_legend,
  device = "png",
  scale = 0.15,
  dpi = 300,
  width = 300, height = 300, units = "mm",
  limitsize = F
)

#### prepare and render plot ####
p0 <- ggplot() +
  theme_bw() +
  xlab("time in years calBC/calAD") +
  ylab("") +
  scale_color_gradientn(
    colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"),
    na.value = NA,
    guide = F
  ) +
  scale_x_continuous(breaks = seq(-7000, 1000, 1000)) +
  coord_cartesian(
    xlim = c(-7000, 1000),
    ylim = c(-30, max(origin_grid_mean$undirected_mean_spatial_distance, na.rm = T))
  )

p4 <- p0 + 
  lemon::facet_rep_wrap(~region_id, ncol = 1, repeat.tick.labels = T) +
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
      ymin = undirected_mean_spatial_distance - 2*se_spatial_distance,
      ymax = undirected_mean_spatial_distance + 2*se_spatial_distance
    ),
    fill = "lightgrey",
  ) +
  geom_line(
    data = moving_origin_grid,
    mapping = aes(x = z, y = undirected_mean_spatial_distance),
    size = 0.4,
    colour = "darkgrey"
  ) +
  geom_line(
    data = moving_origin_grid,
    mapping = aes(x = z, y = undirected_mean_spatial_distance_upper_quartile),
    size = 0.4,
    color = "red"
  ) +
  geom_point(
    data = origin_grid_mean,
    mapping = aes(
      x = mean_search_z, y = undirected_mean_spatial_distance, color = mean_angle_deg
    ),
    alpha = 1,
    size = 2,
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
    size = 1
  )

ggsave(
  filename = "plots/presentation/selected_curves.png",
  plot = p4,
  device = "png",
  scale = 0.4,
  dpi = 500,
  width = 450, height = 400, units = "mm",
  limitsize = F
)

