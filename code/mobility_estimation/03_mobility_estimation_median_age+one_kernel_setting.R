library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")
load("data/gpr/interpol_grid_median.RData")

#### spatial origin ####

interpol_grid_origin <- mobest::search_spatial_origin(interpol_grid, steps = 6)

# gb <- interpol_grid_origin %>%
#   dplyr::filter(
#     region_id == "Iberia"
#   )
# 
# load("data/spatial/extended_area.RData")
# 
# library(ggplot2)
# gb %>% dplyr::filter(
#   #z > -4000 & z < -3000
#   z %% 500 == 0
# ) %>%
#   ggplot() +
#   geom_sf(data = extended_area) +
#   geom_raster(aes(x, y, fill = mean_C1)) +
#   geom_segment(aes(x, y, xend = x_origin, yend = y_origin)) +
#   facet_wrap(~z)

# library(ggplot2)
# interpol_grid_origin %>%
#   dplyr::filter(
#     kernel_setting_id == "ds600_dt300_g001",
#     z == -5500
#   ) %>% ggplot() +
#   geom_segment(
#     aes(x, y, xend = x_origin, yend = y_origin)
#   )

#### mobility proxy ####

mobility_proxy <- mobest::estimate_mobility(interpol_grid_origin)

#mobility <- mobility_proxy
 
save(mobility_proxy, file = "data/mobility_estimation/mobility_proxy_median.RData")

mobility_proxy %>%
  # main
  dplyr::group_by(region_id, z, independent_table_id, kernel_setting_id) %>%
  dplyr::summarise(
    mean_speed_km_per_decade = mean(speed_km_per_decade)#,
    #mean_angle_deg = mobest::mean_deg(angle_deg)
  ) %>%
  ggplot() +
  geom_line(
    aes(
      x = z, y = mean_speed_km_per_decade,
      group = interaction(independent_table_id, kernel_setting_id)#,
      #color = mean_angle_deg
    ),
    alpha = 0.5
  ) +
  facet_grid(cols = dplyr::vars(region_id), rows = dplyr::vars(kernel_setting_id)) +
   scale_color_gradientn(
    colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A")
  )


#### boxplot ####

interpol_grid_origin %>% dplyr::mutate(
  z_cut = cut(z, breaks = seq(-7500, 1500, 500), labels = seq(-7250, 1250, 500))
) %>% ggplot() +
  geom_boxplot(
    aes(
      x = z_cut, y = spatial_distance, group = interaction(z_cut, kernel_setting_id), fill = kernel_setting_id#,
      #group = interaction(independent_table_id, kernel_setting_id),
      #color = angle_deg
    ),
    outlier.size = 0.1
  ) +
  facet_wrap(dplyr::vars(region_id)) +
  coord_cartesian(ylim = c(0, 2000000))

# mobility_proxy %>%
#   dplyr::group_by(kernel_setting_id, region_id) %>%
#   dplyr::arrange(z, .by_group = T) %>%
#   dplyr::mutate(
#     movavg = slider::slide_dbl(mean_km_per_decade, mean, .before = 4, .after = 4)
#   ) %>%
#   ggplot() +
#   geom_line(
#     aes(
#       x = z, y = mean_km_per_decade,
#       group = interaction(independent_table_id, kernel_setting_id),
#       color = angle_deg
#     ),
#     alpha = 0.5
#   ) +
#   geom_line(
#     aes(
#       x = z, y = movavg,
#       group = interaction(independent_table_id, kernel_setting_id),
#     ),
#     color = "blue"
#   ) +
#   #facet_grid(cols = dplyr::vars(region_id), rows = dplyr::vars(kernel_setting_id)) +
#   facet_wrap(dplyr::vars(region_id)) +
#   theme_bw() +
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_text(angle = 40, hjust = 1),
#     strip.background = element_rect(fill = NA)
#   ) +
#   xlab("time calBC/calAD [y]") +
#   ylab("\"Speed\" [km/decade]") +
#   scale_color_gradientn(
#     colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"),
#     guide = F
#   ) +
#   scale_x_continuous(breaks = c(-7000, -5000, -3000, -1000, 1000))

