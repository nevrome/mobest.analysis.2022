library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")
load("data/plot_reference_data/no_data_windows.RData")
load("data/plot_reference_data/no_data_windows_yearwise.RData")

mobility <- lapply(
  list.files("data/mobility_estimation/age_resampling+origin_search+directed_speed+one_kernel_setting", full.names = T),
  function(x) {
    load(x)
    mobility_proxy
  }
) %>% dplyr::bind_rows()

mobility$region_id = factor(mobility$region_id, levels = c(
  "Britain and Ireland",
  "Southern Scandinavia",
  "Baltics",
  "Eastern Europe",
  "France",
  "Central Europe",
  "Southeastern Europe",
  "Caucasus",
  "Iberia",
  "Italy",
  "Turkey",
  "Levant"
))

# moving average
mean_mobility <- mobility %>%
  dplyr::group_by(independent_table_id, kernel_setting_id, region_id, z) %>%
  dplyr::summarise(
    mean_speed_km_per_decade = sqrt(mean(x_to_origin)^2 + mean(y_to_origin)^2)/1000/unique(abs(.data[["z"]]-.data[["z_origin"]]))*10,
    mean_angle_deg = mobest::vec2deg(c(mean(x_to_origin_norm), mean(y_to_origin_norm)))
  ) %>%
  dplyr::filter(
    !is.na(region_id)
  )

#### mobility estimator curves ####

p_estimator <- mean_mobility %>%
  ggplot() +
  geom_line(
    aes(
      x = z, y = mean_speed_km_per_decade,
      group = interaction(independent_table_id, kernel_setting_id),
      color = mean_angle_deg#,
      #linetype = kernel_setting_id
    ),
    size = 0.2
  ) +
  # geom_ribbon(
  #   data = mean_mobility,
  #   aes(
  #     x = z, ymin = mean_speed_km_per_decade - sd_speed_km_per_decade, ymax = mean_speed_km_per_decade + sd_speed_km_per_decade,
  #     group = interaction(independent_table_id, kernel_setting_id),
  #     fill = kernel_setting_id
  #   ),
  #   alpha = 0.1,
  #   color = "black", size = 0.1
  # ) +
  geom_point(
    data = janno_final,
    aes(x = Date_BC_AD_Median_Derived, y = 0),
    shape = "|"
  ) +
  geom_rect(
    data = no_data_windows,
    aes(
      xmin = min_date_not_covered, xmax = max_date_not_covered,
      ymin = -300, ymax = 500
    ),
    alpha = 0.3, fill = "red"
  ) +
  geom_vline(
    data = data.frame(x = c(-5500, -2800, 100)),
    aes(xintercept = x),
    linetype = "dotted"
  ) +
  facet_wrap(dplyr::vars(region_id)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 40, hjust = 1)
  ) +
  xlab("time in years calBC/calAD") +
  ylab("\"Speed\" [km/decade]") +
  scale_color_gradientn(
    colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"),
    guide = F
  ) +
  scale_x_continuous(breaks = seq(-7000, 1000, 1000)) +
  coord_cartesian(ylim = c(-0, max(mean_mobility$mean_speed_km_per_decade, na.rm = T))) +
  xlab("")

#### map series ####

load("data/spatial/mobility_regions.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg102013.RData")

ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

mobility_maps <- mean_mobility %>% 
  dplyr::filter(z %in% c(-5500, -2800, 100)) %>%
  dplyr::group_by(region_id, z) %>%
  dplyr::summarise(
    mean_mean_km_per_decade = mean(mean_speed_km_per_decade),
    mean_mean_angle_deg = mobest::mean_deg(mean_angle_deg),
    mean_mean_angle_deg_text = 360 - mean_mean_angle_deg
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    z_named = dplyr::recode_factor(as.character(z), !!!list(
      "-5500" = "-5600 ⬳ -5500 calBC", 
      "-2800" = "-2900 ⬳ -2800 calBC", 
      "100" = "0 calBC/calAD ⬳ 100 calAD"
    ))
  ) %>%
  dplyr::left_join(
    no_data_windows_yearwise %>% 
      dplyr::filter(date_not_covered %in% c(-5500, -2800, 100)) %>% 
      unique %>%
      dplyr::mutate(not_covered = TRUE), 
    by = c("region_id", "z" = "date_not_covered")
  ) %>% 
  dplyr::mutate(
    not_covered = tidyr::replace_na(not_covered, FALSE)
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
    aes(fill = not_covered, alpha = not_covered),
    color = "black",
    size = 0.4
  ) +
  geom_text(
    data = mobility_maps_center,
    mapping = aes(
      x = x, y = y, 
      color = mean_mean_angle_deg, 
      angle = mean_mean_angle_deg_text,
      size = mean_mean_km_per_decade
    ),
    label="\u2191"
  ) +
  scale_size_continuous(
    range = c(3, 12), name = "mean \"Speed\" [km/decade]",
    breaks = round(diff(range(mobility_maps_center$mean_mean_km_per_decade))/5)*(1:5),
    guide = guide_legend(nrow = 1, label.position = "bottom")
  ) +
  scale_color_gradientn(
    colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"), 
    guide = F
  ) +
  scale_fill_manual(
    values = c("TRUE" = "red", "FALSE" = "white"), 
    guide = FALSE
  ) +
  scale_alpha_manual(
    values = c("TRUE" = 0.3, "FALSE" = 0.8), 
    guide = FALSE
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
    crs = epsg102013
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
  paste0("plots/figure_5_mobility_curves_age_resampling+origin_search+directed_speed+one_kernel_setting.png"),
  plot = p,
  device = "png",
  scale = 0.5,
  dpi = 300,
  width = 700, height = 400, units = "mm",
  limitsize = F
)

