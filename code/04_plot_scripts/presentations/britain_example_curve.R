library(magrittr)
library(ggplot2)

#### data ####

# curves
load("data/genotype_data/janno_final.RData")
load("data/origin_search/packed_origin_vectors.RData")
load("data/origin_search/origin_summary.RData")
load("data/origin_search/no_data_windows.RData")

# filter and prep daza

janno_final %<>% dplyr::filter(region_id == "Britain and Ireland")
packed_origin_vectors <- packed_origin_vectors %>%
  dplyr::filter(multivar_method == "pca5", search_time == -667) %>%
  dplyr::filter(region_id == "Britain and Ireland")
origin_summary <- origin_summary %>%
  dplyr::filter(multivar_method == "pca5", search_time == -667) %>%
  dplyr::filter(region_id == "Britain and Ireland")
no_data_windows <- no_data_windows %>%
  dplyr::filter(multivar_method == "pca5", search_time == -667) %>%
  dplyr::filter(region_id == "Britain and Ireland")

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
  geom_errorbarh(
    data = packed_origin_vectors_time,
    mapping = aes(
      y = ov_dist,
      xmax = Date_BC_AD_Stop_Derived,
      xmin = Date_BC_AD_Start_Derived,
      color = ov_angle_deg
    ),
    alpha = 1,
    size = 0.2,
    height = 40
  ) +
  geom_errorbar(
    data = packed_origin_vectors,
    mapping = aes(
      x = search_z, 
      ymax = ov_dist + ov_dist_sd,
      ymin = ov_dist - ov_dist_sd,
      color = ov_angle_deg
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
    data = packed_origin_vectors,
    mapping = aes(
      x = search_z,
      y = ov_dist,
      color = ov_angle_deg
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
      packed_origin_vectors %>% dplyr::right_join(lookup, by = "search_id")
    },
    mapping = aes(
      x = search_z, y = ov_dist, label = label_name
    ),
    ylim = c(2500, NA),
    segment.size      = 0.3,
    segment.curvature = 0.3,
    segment.square    = FALSE,
    arrow = arrow(length = unit(0.015, "npc")),
    min.segment.length = unit(0.015, "npc"),
    point.padding = 20,
    size = 5,
    alpha = 1
  ) +
  geom_point(
    data = janno_final %>% dplyr::filter(region_id %in% unique(origin_summary$region_id)),
    aes(x = Date_BC_AD_Median_Derived, y = -100),
    shape = "|",
    size = 3
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 13)
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
    ylim = c(-30, max(packed_origin_vectors$ov_dist, na.rm = T) - 100)
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
    x = -0.035, y = 0.6, 
    width = 0.37, height = 0.37
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

