library(magrittr)
library(ggplot2)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")

ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

# map
p_map <- ggplot() +
  geom_sf(
    data = extended_area,
    fill = "white", colour = "black", size = 0.4
  ) +
  geom_sf(
    data = research_area,
    fill = NA, colour = "black", size = 0.8, linetype = "dashed"
  ) +
  geom_point(
    data = anno,
    aes(x = x, y = y, color = region_id, shape = age_group_id),
    size = 2
  ) +
  theme_bw() +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = sf::st_crs("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
  ) + 
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(colour = "grey", size = 0.3),
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.title = element_text(size = 13),
    legend.spacing.x = unit(0.2, 'cm'),
    legend.key.height = unit(0.4, 'cm')
  ) +
  scale_color_manual(
    values = c(
      "Central Europe" = "#999999", 
      "Iberia" = "#E69F00", 
      "Eastern Europe" = "#56B4E9", 
      "Britain and Ireland" = "#009E73", 
      "Turkey" = "#871200",
      "France" = "#F0E442", 
      "Near East" = "#0072B2", 
      "Caucasus" = "#D55E00", 
      "Italy" = "#CC79A7", 
      "Southeastern Europe" = "#2fff00"
    )
  ) +
  scale_shape_manual(
    values = c(
      ">-8000" = 15,
      "-8000 - -6000" = 15,
      "-6000 - -4000" = 17,
      "-4000 - -2000" = 6,
      "-2000 - 0" = 4
    )
  ) +
  guides(
    color = guide_legend(title = "Region", nrow = 4, ncol = 3),
    shape = guide_legend(title = "Time", ncol = 1)
  )

# 3D plot
theta = -10 
phi = 20

p_3D <- ggplot(
  data = anno,
  aes(x = x/1000, y = y/1000, z = calage_center, color = region_id, shape = age_group_id)
) +
  gg3D::axes_3D(theta = theta, phi = phi) +
  gg3D::stat_3D(theta = theta, phi = phi) +
  scale_shape_manual(
    values = c(
      ">-8000" = 15,
      "-8000 - -6000" = 15,
      "-6000 - -4000" = 17,
      "-4000 - -2000" = 6,
      "-2000 - 0" = 4
    ),
    guide = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Central Europe" = "#999999", 
      "Iberia" = "#E69F00", 
      "Eastern Europe" = "#56B4E9", 
      "Britain and Ireland" = "#009E73", 
      "Turkey" = "#871200",
      "France" = "#F0E442", 
      "Near East" = "#0072B2", 
      "Caucasus" = "#D55E00", 
      "Italy" = "#CC79A7", 
      "Southeastern Europe" = "#2fff00"
    ),
    guide = FALSE
  ) +
  # gg3D::axis_labs_3D(
  #   theta = theta, phi = phi, size = 3,
  #   hjust = c(1,1,1.2,1.2,1.2,1.2),
  #   vjust = c(-.5,-.5,-.2,-.2,1.2,1.2)
  # ) +
  gg3D::labs_3D(
    theta = theta, phi = phi, 
    hjust = c(1,-0.2,-0.2), vjust = c(1.5,1,-.2),
    labs = c("x", "y", "time")
  ) +
  theme_void()
  
# temporal distribution

summed_normalized_probability_distribution <- anno$age_prob_distribution_BC %>%
  dplyr::bind_rows() %>%
  dplyr::group_by(age) %>%
  dplyr::summarise(
    som = sum(norm_dens)
  )

p_tempdist <- summed_normalized_probability_distribution %>%
  ggplot() +
  geom_line(
    aes(x = age, y = som)
  ) +
  theme_bw() +
  xlab("time in years calBC/AD") +
  ylab("sum of normalized post-cal distribution") +
  theme(
    axis.title = element_text(size = 9)
  )

# merge plots

right <- cowplot::plot_grid(p_3D, p_tempdist, ncol = 1, labels = c("B", "C"), hjust = 0.6, vjust = 0.8)

p <- cowplot::plot_grid(p_map, right, ncol = , rel_widths = c(1, 0.45), labels = c("A", NA), scale = 0.97)

ggsave(
  paste0("plots/figure_1_temporal_and_spatial_distribution_of_input_data.tiff"),
  plot = p,
  device = "tiff",
  scale = 0.5,
  dpi = 300,
  width = 550, height = 300, units = "mm",
  limitsize = F
)

