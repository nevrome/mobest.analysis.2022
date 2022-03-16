library(magrittr)
library(ggplot2)

load("data/genotype_data/janno_final.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
load("data/plot_reference_data/region_id_shapes.RData")
load("data/plot_reference_data/age_colors_gradient.RData")

`%=NA=%` <- function(a, b) {
  c <- a == b
  c <- ifelse(is.na(c), FALSE, c)
}

janno_mod <- janno_final %>% 
  dplyr::arrange(Date_BC_AD_Median_Derived) %>%
  dplyr::mutate(
    central_europe = region_id %=NA=% "Central Europe"
  )

# map
p_map <- ggplot() +
  geom_sf(
    data = extended_area,
    fill = "white", colour = "darkgrey", size = 0.4
  ) +
  geom_sf(
    data = research_area,
    fill = NA, colour = "black", size = 0.8, linetype = "dashed"
  ) +
  geom_jitter(
    data = janno_mod %>% dplyr::filter(!central_europe),
    aes(x = x, y = y),
    color = "darkgrey",
    alpha = 1,
    width = 60000,
    height = 60000,
    shape = 3
  ) +
  geom_jitter(
    data = janno_mod %>% dplyr::filter(central_europe),
    aes(x = x, y = y, color = Date_BC_AD_Median_Derived),
    size = 2,
    alpha = 1,
    width = 60000,
    height = 60000,
    shape = 16
  ) +
  theme_bw() +
  coord_sf(
    expand = FALSE,
    crs = sf::st_crs(epsg3035)
  ) + 
  theme(
    axis.title = element_blank(),
    legend.position = "left",
    legend.direction = "vertical",
    legend.background = element_blank(),
    legend.title = element_text(size = 13),
    legend.spacing.y = unit(0.2, 'cm'),
    legend.key.height = unit(0.4, 'cm'),
    legend.text = element_text(size = 10),
    panel.background = element_rect(fill = "#BFD5E3"),
    plot.margin = unit(c(5.5, 1, 5.5, 5.5), "points")
  ) +
  age_colors_gradient +
  guides(
    color = guide_colorbar(title = "Time", barwidth = 1.5, barheight = 20)
  )

# space time plot
p_space_time <- ggplot() +
  geom_point(
    data = janno_mod %>% dplyr::filter(!central_europe),
    mapping = aes(
      x = Longitude, y = Date_BC_AD_Median_Derived
    ),
    color = "darkgrey",
    shape = 3
  ) +
  geom_point(
    data = janno_mod %>% dplyr::filter(central_europe),
    mapping = aes(
      x = Longitude, y = Date_BC_AD_Median_Derived, 
      color = Date_BC_AD_Median_Derived
    ),
    shape = 16,
    size = 3
  ) +
  age_colors_gradient +
  theme_bw() +
  xlab("Longitude") +
  ylab("time in years calBC/AD") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 9),
    plot.margin = unit(c(5.5, 0, 5.5, 0), "points"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) +
  scale_y_continuous(breaks = seq(-8000, 2000, 1000)) +
  coord_cartesian(ylim = c(-7800, 1800))

# temporal distribution

p_tempdist <- janno_mod %>%
  ggplot() +
  geom_histogram(
    aes(x = Date_BC_AD_Median_Derived, fill = central_europe),
    breaks = seq(-8000, 2000, 200)
  ) +
  theme_bw() +
  xlab("time in years calBC/AD") +
  ylab("# samples") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "none",
    plot.margin = unit(c(5.5, 5.5, 5.5, 0), "points")
  ) +
  coord_flip(xlim = c(-7800, 1800)) +
  scale_fill_manual(
    values = c(
      "TRUE" = "black",
      "FALSE" = "darkgrey"
    )
  ) +
  scale_x_continuous(breaks = seq(-8000, 2000, 1000)) +
  scale_y_continuous(breaks = c(0, 100, 200))

# merge plots

right <- cowplot::plot_grid(
  p_space_time, p_tempdist, ncol = 2, labels = c(NA, NA), align = "h", axis = "lrtb", rel_widths = c(0.75, 0.25)
)

p <- cowplot::plot_grid(p_map, right, nrow = 1, ncol = 2, rel_widths = c(1, 0.3), scale = 0.97)

ggsave(
  paste0("plots/presentation/map_central_europe.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 600, height = 300, units = "mm",
  limitsize = F
)

