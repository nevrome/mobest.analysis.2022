library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")

mobility <- lapply(
  list.files("data/mobility_estimation/age_resampling", full.names = T),
  function(x) {
    load(x)
    mobility_proxy
  }
) %>% dplyr::bind_rows() %>%
  # kernel selection
  dplyr::mutate(
    kernel_setting_id = dplyr::recode(
      kernel_setting_id, 
      "ds400_dt200_g001" = "400km / 200y", 
      "ds600_dt300_g001" = "600km / 300y",
      "ds800_dt400_g001" = "800km / 400y"
    )
  ) %>%
  dplyr::filter(
    kernel_setting_id == "600km / 300y"
  )

mobility$region_id = factor(mobility$region_id, levels = c(
  "Britain and Ireland",
  "Central Europe",
  "Eastern Europe",
  "Caucasus",
  "France", 
  "Italy",
  "Southeastern Europe",
  "Turkey",
  "Iberia",
  "Near East"
))

# moving average
mean_mobility <- mobility %>%
  dplyr::group_by(kernel_setting_id, region_id, z) %>%
  dplyr::summarise(
    mean_mean_km_per_decade = mean(mean_km_per_decade),
    sd_mean_km_per_decade = sd(mean_km_per_decade)
  ) %>%
  dplyr::group_by(kernel_setting_id, region_id) %>%
  dplyr::arrange(z, .by_group = T) %>%
  dplyr::mutate(
    movavg_mean = slider::slide_dbl(mean_mean_km_per_decade, mean, .before = 8, .after = 8),
    movavg_sd = slider::slide_dbl(sd_mean_km_per_decade, mean, .before = 8, .after = 8)
  ) %>% 
  dplyr::ungroup()

# load("data/gpr/temporal_change_age_resampling.RData")
# get sd value back into the game
# mobility <- mobility %>% 
#   dplyr::left_join(
#     iwrs_age_total %>% dplyr::select(region_id, z, mean_gpr_mean_sd_norm), 
#     by = c("region_id", "z")
#   ) 
# mean_mobility <- mean_mobility %>% 
#   dplyr::left_join(
#     iwrs_age_total %>% dplyr::select(region_id, z, mean_gpr_mean_sd_norm), 
#     by = c("region_id", "z")
#   )

# no-data windows
janno_final %>%
  dplyr::group_by(region_id) %>%
  dplyr::summarise(
    I = setdiff(
      -7500:1500,
      lapply(Date_BC_AD_Median_Derived, function(x) {
        seq(x-200, x+200, 1)
      }) %>% Reduce(union, .)
    )
  ) %>%
  dplyr::ungroup()
  


Date_BC_AD_Median_Derived

#### mobility estimator curves ####

p_estimator <- mobility %>%
  ggplot() +
  geom_line(
    aes(
      x = z, y = mean_km_per_decade, 
      group = independent_table_id, 
      color = angle_deg#, 
      #alpha = 1/mean_gpr_mean_sd_norm
    ),
    size = 0.1
  ) +
  geom_line(
    data = mean_mobility, #%>% dplyr::mutate(
    #  movavg_mean = ifelse(mean_gpr_mean_sd_norm < 0.4, movavg_mean, NA)
    #),
    aes(
      x = z, y = movavg_mean
    ),
    color = "black", size = 0.7
  ) +
  geom_ribbon(
    data = mean_mobility,# %>% dplyr::mutate(
    #  movavg_mean = ifelse(mean_gpr_mean_sd_norm < 0.4, movavg_mean, NA)
    #),
    aes(
      x = z, ymin = movavg_mean - movavg_sd, ymax = movavg_mean + movavg_sd
    ),
    fill = "grey", alpha = 0.4,
    color = "black", size = 0.1
  ) +
  geom_point(
    data = janno_final,
    aes(x = Date_BC_AD_Median_Derived, y = 0),
    shape = "|"
  ) +
  facet_wrap(dplyr::vars(region_id)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 40, hjust = 1)#,
    #strip.background = element_rect(fill = NA)
  ) +
  xlab("time in years calBC/calAD") +
  ylab("\"Speed\" [km/decade]") +
  scale_color_gradientn(
    colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"), 
    guide = F
  ) +
  scale_x_continuous(breaks = c(-7000, -5000, -3000, -1000, 1000)) +
  coord_cartesian(ylim = c(0, max(mean_mobility$movavg_mean, na.rm = T))) +
  xlab("")

# move facets around
g <- ggplotGrob(p_estimator)
g$grobs[[13]] <- g$grobs[[7]]
g$grobs[[7]] <- zeroGrob()
g$grobs[[29]] <- g$grobs[[27]]
g$grobs[[27]] <- zeroGrob()
g$grobs[[65]] <- g$grobs[[63]]
g$grobs[[63]] <- zeroGrob()
g$grobs[[31]] <- g$grobs[[32]]
g$grobs[[33]] <- zeroGrob()
p_estimator2 <- ggplotify::as.ggplot(g) # grid::grid.draw(g)

#### map series ####

load("data/spatial/mobility_regions.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg102013.RData")

ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

mobility_maps <- mobility %>% 
  dplyr::filter(z %in% c(-6800, -5500, -2800, 100)) %>%
  dplyr::mutate(
    z = dplyr::recode_factor(as.character(z), !!!list(
      "-6800" = "-7000 ⬳ -6800 calBC", 
      "-5500" = "-5700 ⬳ -5500 calBC", 
      "-2800" = "-3000 ⬳ -2800 calBC", 
      "100" = "-100 calBC ⬳ 100 calAD"
    ))
  ) %>%
  dplyr::group_by(region_id, z) %>%
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
    breaks = round(diff(range(mobility_maps_center$mean_mean_km_per_decade))/5)*(1:5),
    guide = guide_legend(nrow = 1, label.position = "bottom")
  ) +
  scale_color_gradientn(
    colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"), 
    guide = F
  ) +
  facet_grid(cols = dplyr::vars(z)) +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "#BFD5E3")#,
    #strip.background = element_rect(fill = NA)
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

plot_top <- cowplot::ggdraw(p_estimator2) + cowplot::draw_plot(p_double_legend, 0.35, 0, .35, .35)

p <- cowplot::plot_grid(
  plot_top, p_map, nrow = 2, rel_heights = c(1, 0.44), labels = c("A", "B"),
  label_y = c(1, 1.1)
)

ggsave(
  paste0("plots/figure_5_mobility_estimator.png"),
  plot = p,
  device = "png",
  scale = 0.5,
  dpi = 300,
  width = 700, height = 400, units = "mm",
  limitsize = F
)

