library(magrittr)
library(ggplot2)

load("data/gpr/temporal_change_median.RData")

# iwrs$kernel_setting_id <- factor(iwrs$kernel_setting_id, levels = c(
#   "ds250_dt125_g001",
#   "ds500_dt250_g001", 
#   "ds1000_dt500_g001"
# ))


#### derivative estimator curves ####

p_estimator <- temporal_change %>%
  ggplot() +
  geom_line(
    aes(
      x = z, y = mean_change_combined, 
      color = mean_sd_norm
    )
  ) +
  geom_line(
    aes(
      x = z, y = movavg, alpha = 1 - mean_sd_norm
    ),
    color = "blue", size = 1
  ) +
  facet_grid(cols = dplyr::vars(region_id), rows = dplyr::vars(kernel_setting_id)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 40, hjust = 1),
    strip.background = element_rect(fill = NA)
  ) +
  xlab("time calBC/calAD [y]") +
  ylab("\"Change\" [MDS-distance/50years]") +
  scale_color_gradientn(
    colours = c("red", "grey", "grey")
  ) +
  scale_x_continuous(breaks = c(-7000, -5000, -3000, -1000, 1000)) +
  scale_alpha_continuous(guide = FALSE) +
  coord_cartesian(ylim = c(0, 0.02))

# interpol_grid_with_change %>%
#   dplyr::filter(
#     z %% 200 == 0
#   ) %>%
#   ggplot() +
#   geom_raster(aes(x, y, fill = change_combined)) +
#   # geom_point(
#   #   data = . %>% dplyr::filter(sd_to_high),
#   #   mapping = aes(x, y),
#   #   shape = 4, size = 0.5, color = "red"
#   # ) +
#   facet_wrap(~z) +
#   scale_fill_viridis_c()

#### map series ####

# load("data/spatial/mobility_regions.RData")
# load("data/spatial/research_area.RData")
# load("data/spatial/extended_area.RData")
# load("data/spatial/epsg102013.RData")
# 
# ex <- raster::extent(research_area)
# xlimit <- c(ex[1], ex[2])
# ylimit <- c(ex[3], ex[4])
# 
# mobility_maps <- mobility %>% 
#   dplyr::filter(z %in% c(-5400, -2700, 100)) %>%
#   dplyr::mutate(
#     z = dplyr::recode_factor(as.character(z), !!!list(
#       "-5400" = "-5900 ⬳ -5400 calBC", 
#       "-2700" = "-3200 ⬳ -2700 calBC", 
#       "100" = "-400 calBC ⬳ 100 calAD"
#     ))
#   ) %>%
#   dplyr::group_by(region_id, z) %>%
#   dplyr::summarise(
#     mean_mean_km_per_decade = mean(mean_km_per_decade),
#     mean_angle_deg = mobest::mean_deg(angle_deg %>% na.omit()),
#     mean_angle_deg_text = 360 - mean_angle_deg
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::left_join(
#     mobility_regions,
#     by = "region_id"
#   ) %>% sf::st_as_sf()
# 
# mobility_maps_center <- mobility_maps %>%
#   sf::st_centroid() %>%
#   dplyr::mutate(
#     x = sf::st_coordinates(.)[,1],
#     y = sf::st_coordinates(.)[,2]
#   )
# 
# p_map <- ggplot() +
#   geom_sf(
#     data = extended_area,
#     fill = "white", colour = "black", size = 0.4
#   ) +
#   geom_sf(
#     data = mobility_maps,
#     fill = "white",
#     alpha = 0.8,
#     color = "black",
#     size = 0.4
#   ) +
#   geom_text(
#     data = mobility_maps_center,
#     mapping = aes(
#       x = x, y = y, 
#       color = mean_angle_deg, 
#       angle = mean_angle_deg_text,
#       size = mean_mean_km_per_decade
#     ),
#     label="\u2191"
#   ) +
#   scale_size_continuous(
#     range = c(3, 12), name = "mean \"Speed\" [km/decade]",
#     guide = guide_legend(nrow = 1, label.position = "bottom")
#   ) +
#   scale_color_gradientn(
#     colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"), 
#     guide = F
#   ) +
#   facet_grid(cols = dplyr::vars(z)) +
#   theme_bw() +
#   theme(
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     axis.title = element_blank(),
#     panel.background = element_rect(fill = "#BFD5E3"),
#     strip.background = element_rect(fill = NA)
#   ) +
#   coord_sf(
#     xlim = xlimit, ylim = ylimit,
#     crs = epsg102013
#   )
# 
# #### compile plots ####
# 
# p_bottom_right <- cowplot::plot_grid(p_legend, p_arrows_legend, nrow = 2, rel_heights = c(1, 0.7))
# p_bottom <- cowplot::plot_grid(p_map, p_bottom_right, nrow = 1, rel_widths = c(1, 0.3))
# p <- cowplot::plot_grid(p_estimator, p_bottom, nrow = 2, rel_heights = c(1, 0.5), labels = c("A", "B"))

ggsave(
  paste0("plots/figure_sup_12_derivative.png"),
  plot = p_estimator,
  device = "png",
  scale = 0.5,
  dpi = 300,
  width = 1000, height = 600, units = "mm",
  limitsize = F
)

