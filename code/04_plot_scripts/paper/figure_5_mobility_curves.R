library(magrittr)
library(ggplot2)

#### data ####

# curves
load("data/poseidon_data/janno_final.RData")
load("data/origin_search/origin_grid_mean.RData")
load("data/origin_search/origin_grid_modified.RData")
load("data/origin_search/moving_origin_grid.RData")
load("data/origin_search/no_data_windows.RData")

# maps
load("data/spatial/mobility_regions.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
load("data/plot_reference_data/region_id_shapes.RData")

no_data_windows$region_id <- factor(
  no_data_windows$region_id, levels = levels(mobility_regions$region_id)
)

#### mobility estimator curves ####
p_estimator <- ggplot() +
  lemon::facet_rep_wrap(~region_id, ncol = 2, repeat.tick.labels = T) +
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
  # geom_ribbon(
  #   data = moving_origin_grid,
  #   mapping = aes(
  #     x = z,
  #     ymin = directed_mean_spatial_distance - 2*sd_spatial_distance,
  #     ymax = directed_mean_spatial_distance + 2*sd_spatial_distance
  #   ),
  #   fill = "lightgrey",
  #   alpha = 0.3
  # ) +
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
  # geom_line(
  #   data = moving_origin_grid,
  #   mapping = aes(x = z, y = directed_mean_spatial_distance_upper_quartile),
  #   size = 0.4
  # ) +
  geom_errorbarh(
    data = origin_grid_mean,
    mapping = aes(
      y = directed_mean_spatial_distance, 
      xmax = mean_search_z + sd_search_z,
      xmin = mean_search_z - sd_search_z,
      color = mean_angle_deg
    ),
    alpha = 1,
    size = 0.13,
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
    size = 0.13,
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
    size = 1.8,
    shape = 4
  ) +
  ggrepel::geom_label_repel(
    data = {
      lookup <- tibble::tribble(
        ~search_id, ~label_name,
        "RISE434.SG", "RISE434", 
        "Stuttgart_published.DG", "Stuttgart", 
        "3DT26.SG", "3DRIF-26", 
        "N22.SG", "N22",
        "I15940", "I15940",
        "I3719_published", "I3719",
        "ILK001", "ILK001",
        "ILK002", "ILK002",
        "ILK003", "ILK003",
        "I2352", "I2352",
        # "ATP3.SG", "ATP3", wrong date?
        "I8208", "I8208",
        "I3983", "I3983",
        "MJ-14.SG", "MJ-14",
        "scy009.SG", "scy009",
        "R68.SG", "R68.SG",
        "R67.SG", "R67.SG",
        "I2637", "I2637",
        "I5367", "I5367",
        "VK546.SG", "VK546",
        "XN191", "XN191",
        "XN206", "XN206",
        "I5411", "I5411",
        "RISE1159.SG", "RISE1159",
        "I3948", "I3948",
        "I2534", "I2534",
        "I2787", "I2787",
        "I2741", "I2741",
        "I2163", "I2163",
        "DA191.SG", "DA191",
        "DA194.SG", "DA194",
        "HUN001.merged", "HUN001",
        "I10899_published", "I10899",
        "CB13.SG", "CB13",
        "I0462", "I0462",
        "I8344", "I8344"
      )
      origin_grid_mean %>% dplyr::filter(
        search_id %in% c(lookup$search_id)
      ) %>% dplyr::left_join(lookup)
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
    point.padding = 1,
    size = 3,
    alpha = 0.6
  ) +
  geom_point(
    data = janno_final %>% dplyr::filter(!is.na(region_id)),
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
    guide = F
  ) +
  scale_x_continuous(breaks = seq(-7000, 1000, 1000)) +
  coord_cartesian(
    xlim = c(-7400, 1400),
    ylim = c(-100, 3000) #max(origin_grid_mean$directed_mean_spatial_distance, na.rm = T))
  )

#### direction legend ####

p_legend <- tibble::tibble(
  ID = 1:360,
  angle_start = 0:359,
  angle_stop = 1:360
) %>%
  ggplot() + 
  geom_rect(
    aes(xmin = 2.8, xmax = 3.8, ymin = angle_start, ymax = angle_stop, fill = ID)
  ) +
  scale_fill_gradientn(
    colours = c("#F5793A", "#85C0F9", "#A95AA1", "#33a02c", "#F5793A"),
    na.value = NA,
    guide = F
  ) +
  # scale_fill_manual(
  #   values = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"), 
  #   guide = FALSE
  # ) +
  coord_polar(theta = "y") +
  xlim(2.0, 4.1) +
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

p <- cowplot::ggdraw(p_estimator) +
  cowplot::draw_plot(
    p_legend,
    x = 0.06, y = 0.76, 
    width = 0.18, height = 0.20
  )

ggsave(
  paste0("plots/figure_5_mobility_curves6.png"),
  plot = p,
  device = "png",
  scale = 0.7,
  dpi = 300,
  width = 430, height = 300, units = "mm",
  limitsize = F
)

