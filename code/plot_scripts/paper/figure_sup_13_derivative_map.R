library(magrittr)
library(ggplot2)

load("data/gpr/interpol_grid_median_with_change.RData")

interpol_grid_with_change %>%
  dplyr::filter(
    kernel_setting_id == "ds400_dt200_g001",
    z %% 500 == 0
  ) %>%
  ggplot() +
  geom_raster(aes(x, y, fill = change_combined)) +
  geom_point(
    data = . %>% dplyr::filter(mean_sd_norm > 0.13),
    mapping = aes(x, y),
    shape = 4, size = 0.5, color = "red"
  ) +
  facet_wrap(~z) +
  scale_fill_viridis_c()

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