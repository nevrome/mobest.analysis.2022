library(magrittr)
library(ggplot2)

mobility <- lapply(
  list.files("data/mobility_estimation_main_run", full.names = T),
  function(x) {
    load(x)
    mobility_proxy
  }
) %>% dplyr::bind_rows()

mobility$kernel_setting_id <- factor(mobility$kernel_setting_id, levels = c(
  "ds100_dt100_g001",
  "ds200_dt200_g001",
  "ds500_dt500_g001",
  "ds1000_dt1000_g001",
  "ds2000_dt2000_g001"
))

p_estimator <- mobility %>%
  # dplyr::mutate(
  #   kernel_setting_id = dplyr::recode(
  #     kernel_setting_id, "ds100_dt200_g01" = "small kernel", "ds200_dt400_g01" = "big kernel"
  #   )
  # ) %>%
  ggplot() +
  geom_line(
    aes(
      x = z, y = mean_km_per_decade, 
      group = interaction(independent_table_id, kernel_setting_id), 
      color = angle_deg
    ),
    alpha = 0.3
  ) +
  facet_grid(cols = dplyr::vars(region_id), rows = dplyr::vars(kernel_setting_id)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  xlab("time calBC [y]") +
  ylab("\"Speed\" [km/decade]") +
  # guides(
  #   color = guide_legend(title = "kernels", override.aes = list(size = 10, alpha = 1))
  # ) +
  # scale_color_manual(
  #   values = c(
  #     "small kernel" = "orange",
  #     "big kernel" = "darkgreen"
  #   )
  # ) +
  # scale_fill_gradient2(
  #   low = "blue",
#   mid = "red",
#   high = "blue",
#   midpoint = 180
# ) +
scale_color_gradientn(colours = c("blue", "red", "yellow", "green", "blue"), guide = F) +
  NULL

load("data/spatial/mobility_regions.RData")

mobility_maps <- mobility %>% 
  dplyr::mutate(
    z_cut = cut(z, breaks = c(-7500, -6000, -5000, -4000, -3000, -2000, -1000, 0))
  ) %>% 
  dplyr::group_by(region_id, z_cut) %>%
  dplyr::summarise(
    mean_mean_km_per_decade = mean(mean_km_per_decade),
    mean_angle_deg = CircStats::deg(CircStats::circ.mean(CircStats::rad(angle_deg %>% na.omit())))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    mobility_regions,
    by = "region_id"
  ) %>% sf::st_as_sf()

mobility_maps_center <- mobility_maps %>%
  sf::st_centroid()

p_map <- ggplot() +
  geom_sf(
    data = mobility_maps,
    mapping = aes(fill = mean_mean_km_per_decade)
  ) +
  geom_sf(
    data = mobility_maps_center,
    mapping = aes(color = mean_angle_deg)
  ) +
  facet_grid(cols = dplyr::vars(z_cut)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_gradientn(colours = c("blue", "red", "yellow", "green", "blue"))


p <- cowplot::plot_grid(p_estimator, p_map, nrow =2, rel_heights = c(1, 0.6))

ggsave(
  paste0("plots/figure_5_mobility_estimator.png"),
  plot = p,
  device = "png",
  scale = 0.5,
  dpi = 300,
  width = 550, height = 350, units = "mm",
  limitsize = F
)

