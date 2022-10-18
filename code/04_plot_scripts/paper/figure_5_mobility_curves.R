library(magrittr)
library(ggplot2)

load("data/genotype_data/janno_final.RData")
load("data/origin_search/packed_origin_vectors.RData")
load("data/origin_search/origin_summary.RData")
load("data/origin_search/no_data_windows.RData")
individuals <- readr::read_csv(
  "code/04_plot_scripts/paper/individuals_to_highlight.csv",
  col_types = "cc",
  comment = "#"
)

filter_setting <- function(x) {
  x %>%
    dplyr::filter(multivar_method == "mds2", search_time == -667) %>%
    dplyr::filter(region_id %in% c("Britain and Ireland", "Central Europe", "Iberia", "Italy"))
}

packed_origin_vectors %<>% filter_setting()
origin_summary %<>% filter_setting()
no_data_windows %<>% filter_setting()

no_data_windows$region_id <- factor(
  no_data_windows$region_id, levels = levels(janno_final$region_id)
)

lookup <- individuals %>% dplyr::inner_join(packed_origin_vectors, by = "search_id")

packed_origin_vectors_time <- packed_origin_vectors %>%
  dplyr::left_join(
    janno_final %>% dplyr::select(
      search_id = Poseidon_ID,
      Date_BC_AD_Median_Derived,
      Date_BC_AD_Prob
    ),
    by = "search_id"
  ) %>%
  dplyr::bind_cols(
    .,
    purrr::map_dfr(.$Date_BC_AD_Prob, function(x) {
      start_stop <- x %>% dplyr::filter(two_sigma) %>% dplyr::slice(c(1,dplyr::n()))
      tibble::tibble(
        Date_BC_AD_Start_Derived = start_stop$age[1],
        Date_BC_AD_Stop_Derived = start_stop$age[2]
      )
    })
  ) %>%
  dplyr::select(-Date_BC_AD_Prob)

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
    data = origin_summary,
    mapping = aes(
      x = z,
      ymin = ov_dist - 2*ov_dist_se,
      ymax = ov_dist + 2*ov_dist_se
    ),
    fill = "lightgrey",
  ) +
  geom_line(
    data = origin_summary,
    mapping = aes(x = z, y = ov_dist),
    size = 0.4,
    colour = "darkgrey"
  ) +
  # geom_line(
  #   data = moving_origin_grid,
  #   mapping = aes(x = z, y = directed_mean_spatial_distance_upper_quartile),
  #   size = 0.4
  # ) +
  geom_errorbarh(
    data = packed_origin_vectors_time,
    mapping = aes(
      y = ov_dist,
      xmax = Date_BC_AD_Stop_Derived,
      xmin = Date_BC_AD_Start_Derived,
      color = ov_angle_deg
    ),
    alpha = 0.5,
    size = 0.3,
    height = 40
  ) +
  geom_errorbar(
    data = packed_origin_vectors_time,
    mapping = aes(
      x = Date_BC_AD_Median_Derived, 
      ymax = ov_dist + ov_dist_sd,
      ymin = ov_dist - ov_dist_sd,
      color = ov_angle_deg
    ),
    alpha = 0.5,
    size = 0.3,
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
    data = packed_origin_vectors_time,
    mapping = aes(
      x = Date_BC_AD_Median_Derived,
      y = ov_dist,
      color = ov_angle_deg
    ),
    alpha = 1,
    size = 1.8,
    shape = 4
  ) +
  ggrepel::geom_label_repel(
    data = lookup,
    mapping = aes(
      x = search_z, y = ov_dist, label = label_name
    ),
    # nudge_y + direction manage the fixed position of the labels
    nudge_y = 2900 - lookup$ov_dist,
    direction = "x",
    segment.size      = 0.4,
    segment.curvature = 0.3,
    segment.square    = FALSE,
    arrow = arrow(length = unit(0.02, "npc")),
    min.segment.length = unit(0.02, "npc"),
    point.padding = 1,
    label.padding = 0.3,
    size = 3,
    alpha = 0.35,
    seed = 345
  ) +
  geom_point(
    data = janno_final %>% dplyr::filter(region_id %in% unique(origin_summary$region_id)),
    aes(x = Date_BC_AD_Median_Derived, y = -100),
    shape = "|"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  xlab("time in years calBC/calAD") +
  ylab("length of \"mobility vector\" (directed mean) in km") +
  scale_color_gradientn(
    colours = c("#F5793A", "#85C0F9", "#A95AA1", "#33a02c", "#F5793A"),
    na.value = NA,
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(-7000, 1000, 1000)) +
  coord_cartesian(
    xlim = c(-7400, 1400),
    ylim = c(-100, 2700) #max(origin_grid_mean$directed_mean_spatial_distance, na.rm = T))
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
    guide = "none",
    limits = c(0,360)
  ) +
  # scale_fill_manual(
  #   values = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"), 
  #   guide = "none"
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
    x = -0.01, y = 0.65, 
    width = 0.3, height = 0.3
  )

ggsave(
  paste0("plots/figure_5_mobility_curves.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.6,
  dpi = 300,
  width = 500, height = 250, units = "mm",
  limitsize = F
)

