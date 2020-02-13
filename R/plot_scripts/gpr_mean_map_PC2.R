library(magrittr)
library(ggplot2)

load("data/gpr/pred_grid_spatial_cropped.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")

ex <- raster::extent(research_area)
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

#### plot resulting model ####

p_PC2 <- ggplot() +
  geom_sf(data = extended_area, fill = "white") +
  geom_raster(
    data = pred_grid_spatial_cropped,# %>% dplyr::filter(pred_bt_s2 < 1),
    aes(x = x_real, y = y_real, fill = pred_PC2_mean, alpha = pred_PC2_s2)
  ) +
  geom_sf(
    data = research_area,
    fill = NA, colour = "red", size = 0.4
  ) +
  scale_fill_viridis_c(
    na.value = NA,
    oob = scales::squish,
    option = "plasma"
  ) +
  scale_alpha_continuous(range = c(1, 0), na.value = 0) +
  theme_bw() +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = sf::st_crs("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
  ) +
  guides(
    fill = guide_colorbar(title = "PC2 prediction", barwidth = 25),
    alpha = guide_legend(title = "SD", override.aes = list(size = 10), nrow = 1, byrow = TRUE, order = 2)
  ) +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 20, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(size = 20),
    panel.grid.major = element_line(colour = "grey", size = 0.3),
    strip.text.x = element_text(size = 20)
  ) +
  facet_wrap(
    ~age_sample, 
    nrow = 3
  )

p_PC2 %>%
  ggsave(
    "plots/gpr_mean_map_PC2.png",
    plot = .,
    device = "png",
    scale = 1,
    dpi = 300,
    width = 550, height = 260, units = "mm",
    limitsize = F
  )
