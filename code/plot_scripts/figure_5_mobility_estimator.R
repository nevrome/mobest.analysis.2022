library(magrittr)
library(ggplot2)

mobility <- lapply(
  list.files("data/mobility_estimation", full.names = T),
  function(x) {
    load(x)
    mobility_proxy
  }
) %>% dplyr::bind_rows() %>%
  # kernel selection
  dplyr::filter(
    !(kernel_setting_id %in% c("ds1000_dt1000_g001", "ds2000_dt2000_g001"))
  ) %>%
  dplyr::mutate(
    kernel_setting_id = dplyr::recode(
      kernel_setting_id, 
      "ds100_dt100_g001" = "100km / 100y", 
      "ds200_dt200_g001" = "200km / 200y",
      "ds500_dt500_g001" = "500km / 500y"
    )
  )

mobility$kernel_setting_id = factor(mobility$kernel_setting_id, levels = c(
  "100km / 100y",
  "200km / 200y",
  "500km / 500y"
))

mobility$region_id = factor(mobility$region_id, levels = c(
  "Britain and Ireland",
  "France", 
  "Iberia",
  "Italy",
  "Central Europe",
  "Eastern Europe",
  "Southeastern Europe",
  "Turkey",
  "Caucasus",
  "Near East"
))

#### mobility estimator curves ####

p_estimator <- mobility %>%
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
    axis.text.x = element_text(angle = 20, hjust = 1),
    strip.background = element_rect(fill = NA)
  ) +
  xlab("time calBC [y]") +
  ylab("\"Speed\" [km/decade]") +
  scale_color_gradientn(
    colours =  c("orange", "#47A649", "#47A649", "red", "red", "#0072B2", "#0072B2", "orange"), 
    guide = F
  )

#### map series ####

load("data/spatial/mobility_regions.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")

ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

mobility_maps <- mobility %>% 
  dplyr::mutate(
    z_cut = cut(z, breaks = c(-7500, -5000, -3000, 0), labels = c("-7500 - -5000 calBC", "-5000 - -3000 calBC", "-3000 - 0 calBC"))
  ) %>% 
  dplyr::group_by(region_id, z_cut) %>%
  dplyr::summarise(
    mean_mean_km_per_decade = mean(mean_km_per_decade),
    mean_angle_deg = mobest::mean_deg(angle_deg %>% na.omit()),
    mean_angle_deg_text = 360 - mean_angle_deg
  ) %>%
  dplyr::ungroup() %>%
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
    fill = "white",
    alpha = 0.8,
    color = "black",
    size = 0.4
  ) +
  geom_text(
    data = mobility_maps_center,
    mapping = aes(
      x = x, y = y, 
      color = mean_angle_deg, 
      angle = mean_angle_deg_text,
      size = mean_mean_km_per_decade
    ),
    label="\u2191"
  ) +
  scale_size_continuous(
    range = c(3, 12), name = "mean \"Speed\" [km/decade]",
    guide = guide_legend(nrow = 1, label.position = "bottom")
  ) +
  scale_color_gradientn(
    colours =  c("orange", "#47A649", "#47A649", "red", "red", "#0072B2", "#0072B2", "orange"), 
    guide = F
  ) +
  facet_grid(cols = dplyr::vars(z_cut)) +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "#BFD5E3"),
    strip.background = element_rect(fill = NA)
  ) +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = sf::st_crs("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
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
    values =  c("orange", "#47A649", "#47A649", "red", "red", "#0072B2", "#0072B2", "orange"), 
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

p_bottom_right <- cowplot::plot_grid(p_legend, p_arrows_legend, nrow = 2, rel_heights = c(1, 0.7))
p_bottom <- cowplot::plot_grid(p_map, p_bottom_right, nrow = 1, rel_widths = c(1, 0.3))
p <- cowplot::plot_grid(p_estimator, p_bottom, nrow = 2, rel_heights = c(1, 0.5), labels = c("A", "B"))

ggsave(
  paste0("plots/figure_5_mobility_estimator.png"),
  plot = p,
  device = "png",
  scale = 0.5,
  dpi = 300,
  width = 700, height = 350, units = "mm",
  limitsize = F
)

