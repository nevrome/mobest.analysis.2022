library(ggplot2)

load("data/pri_ready.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")

ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

pri <- pri_ready %>%
  dplyr::filter(
    # only look at age center data
    independent_table_id == "age_center",
    # only look at big kernel
    kernel_setting_id == "ds200_dt400_g01",
    # only timesteps every 500 years 
    age_sample %in% c(-6500, -5500, -4500, -3500, -2500, -1500)
  ) %>%
  dplyr::mutate(
    spatial_distance_km = spatial_distance/1000
  )

plot <- ggplot() +
  geom_sf(
    data = extended_area,
    fill = "white"
  ) +
  geom_spoke(
    data = pri %>% dplyr::filter(!((x_real == x_real_origin) & (y_real == y_real_origin))),
    mapping = aes(x = x_real, y_real, angle = angle, size = spatial_distance, color = spatial_distance_km),
    radius = 200000,
    arrow = arrow(length = unit(.05, 'inches'))
  ) +
  facet_wrap(
    ~age_sample,
    nrow = 2
  ) +
  scale_size_continuous(
    guide = FALSE,
    range = c(0.2, 0.8)
  ) +
  theme_bw() +
  scale_color_gradient(
    low = "blue",
    high = "red"
  ) +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 20, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(size = 20),
    panel.grid.major = element_line(colour = "grey", size = 0.3),
    strip.text.x = element_text(size = 20)
  ) +
  guides(
    color = guide_colorbar(title = "spatial distance [km]", barwidth = 50),
    alpha = FALSE
  ) +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = sf::st_crs("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
  )

plot %>%
  ggsave(
    paste0("plots/map_timeslices_windfield.jpeg"),
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 550, height = 260, units = "mm",
    limitsize = F
  )

