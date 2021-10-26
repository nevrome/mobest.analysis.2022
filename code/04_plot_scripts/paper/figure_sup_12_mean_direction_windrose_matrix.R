library(magrittr)
library(ggplot2)

#### data ####

load("data/origin_search/origin_grid_modified.RData")

#### map series ####

mobility_maps <- origin_grid_modified %>% 
  dplyr::select(region_id, search_z_cut, angle_deg, spatial_distance) %>%
  dplyr::mutate(
    angle_deg_cut = as.numeric(as.character(
      cut(origin_grid_modified$angle_deg, breaks = seq(0,360,45), labels = seq(22.5,360,45))
    ))
  ) %>%
  dplyr::filter(search_z_cut %in% seq(-6000, 1000, 1000)) %>%
  dplyr::mutate(
    z_named = dplyr::recode_factor(as.character(search_z_cut), !!!rev(list(
      "-6000" = "6250-5750 calBC", 
      "-5000" = "5250-4750 calBC",
      "-4000" = "4250-3750 calBC",
      "-3000" = "3250-2750 calBC",
      "-2000" = "2250-1750 calBC",
      "-1000" = "1250-750 calBC",
      "0" = "250 calBC - 250 calAD",
      "1000"  = "750-1250 calAD"
    )))
  )
  
p <- mobility_maps %>%
  ggplot() +
  facet_grid(
    rows = dplyr::vars(z_named), cols = dplyr::vars(region_id), 
    switch = "y") +
  geom_hline(
    data = data.frame(dist = seq(0,3000, 1000)),
    mapping = aes(yintercept = dist),
    size = 0.1
  ) +
  geom_segment(
    data = data.frame(deg = seq(0,315,45)),
    mapping = aes(x = deg, xend = deg),
    y = 0,
    yend = 3000,
    size = 0.1
  ) +
  geom_boxplot(
    aes(angle_deg_cut, spatial_distance, group = angle_deg_cut), 
    width = 45,
    size = 0.3,
    outlier.size = 0.2,
    outlier.color = "red",
    fill = "#fcbab6"
  ) +
  coord_polar() +
  scale_y_continuous(
    limits = c(-1000, 3000),
    breaks = c(0,1000,2000,3000),
    oob = scales::squish,
    position = "right"
  ) +
  scale_x_continuous(
    breaks = seq(0,315,45),
  ) +
  theme_bw() +
  ylab("Spatial distance to \"origin\" point in [km]") +
  xlab(NULL) +
  theme(
    panel.grid = element_blank()
  )
  
ggsave(
  paste0("plots/figure_sup_12_direction_windrose_matrix.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 1,
  dpi = 300,
  width = 240, height = 300, units = "mm",
  limitsize = F
)
