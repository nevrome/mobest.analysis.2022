library(magrittr)
library(ggplot2)

#### data ####

# curves
load("data/poseidon_data/janno_final.RData")
load("data/origin_search/origin_grid_mean.RData")
load("data/origin_search/moving_origin_grid.RData")
load("data/origin_search/no_data_windows.RData")

# filter for central europe

janno_final %<>% dplyr::filter(region_id == "Britain and Ireland")
origin_grid_mean %<>% dplyr::filter(region_id == "Britain and Ireland")
moving_origin_grid %<>% dplyr::filter(region_id == "Britain and Ireland")
no_data_windows %<>% dplyr::filter(region_id == "Britain and Ireland")

#### mobility estimator curves ####

p_estimator <- ggplot() +
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
      ymin = directed_mean_spatial_distance - 2*se_spatial_distance,
      ymax = directed_mean_spatial_distance + 2*se_spatial_distance
    ),
    fill = "lightgrey",
  ) +
  geom_line(
    data = moving_origin_grid,
    mapping = aes(x = z, y = directed_mean_spatial_distance),
    size = 0.4,
    colour = "darkgrey"
  ) +
  geom_errorbarh(
    data = origin_grid_mean,
    mapping = aes(
      y = directed_mean_spatial_distance, 
      xmax = mean_search_z + sd_search_z,
      xmin = mean_search_z - sd_search_z,
      color = mean_angle_deg
    ),
    alpha = 1,
    size = 0.2,
    height = 40
  ) +
  geom_errorbar(
    data = origin_grid_mean,
    mapping = aes(
      x = mean_search_z, 
      ymax = directed_mean_spatial_distance + undirected_sd_spatial_distance,
      ymin = directed_mean_spatial_distance - undirected_sd_spatial_distance,
      color = mean_angle_deg
    ),
    alpha = 1,
    size = 0.2,
    width = 40
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
    data = origin_grid_mean,
    mapping = aes(
      x = mean_search_z, y = directed_mean_spatial_distance, color = mean_angle_deg
    ),
    alpha = 1,
    size = 3,
    shape = 4
  ) +
  ggrepel::geom_label_repel(
    data = {
      lookup <- tibble::tribble(
        ~search_id, ~label_name,
        "3DT26.SG", "3DRIF-26"
      )
      origin_grid_mean %>% dplyr::right_join(lookup, by = "search_id")
    },
    mapping = aes(
      x = mean_search_z, y = directed_mean_spatial_distance, label = label_name
    ),
    ylim = c(2500, NA),
    segment.size      = 0.3,
    segment.curvature = 0.3,
    segment.square    = FALSE,
    arrow = arrow(length = unit(0.015, "npc")),
    min.segment.length = unit(0.015, "npc"),
    point.padding = 20,
    size = 4,
    alpha = 1
  ) +
  geom_point(
    data = janno_final %>% dplyr::filter(region_id != "Other region"),
    aes(x = Date_BC_AD_Median_Derived, y = -100),
    shape = "|"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  xlab("time [years calBC/calAD]") +
  ylab("spatial distance to \"origin point\" (directed mean) [km]") +
  scale_color_gradientn(
    colours = c("#F5793A", "#85C0F9", "#A95AA1", "#33a02c", "#F5793A"),
    na.value = NA,
    guide = "none",
    limits = c(0,360)
  ) +
  scale_x_continuous(breaks = seq(-7000, 1000, 1000)) +
  coord_cartesian(
    xlim = c(-5000, 1000),
    ylim = c(-30, max(origin_grid_mean$undirected_mean_spatial_distance, na.rm = T))
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
    guide = "none"
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

p <- cowplot::ggdraw(p_estimator) +
  cowplot::draw_plot(
    p_legend,
    x = -0.03, y = 0.55, 
    width = 0.4, height = 0.4
  )

ggsave(
  "plots/presentation/britain_example.png",
  plot = p,
  device = "png",
  scale = 0.35,
  dpi = 500,
  width = 850, height = 400, units = "mm",
  limitsize = F
)

