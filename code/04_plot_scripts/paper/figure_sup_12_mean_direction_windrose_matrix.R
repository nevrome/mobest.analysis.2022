library(magrittr)
library(ggplot2)

#### data ####

load("data/origin_search/packed_origin_vectors.RData")

packed_origin_vectors %<>%
  dplyr::filter(multivar_method == "mds2", search_time == -667) %>%
  dplyr::filter(region_id != "Other region")

#### map series ####
age_groups_limits <- seq(-7500, 1500, 1000)

mobility_maps <- packed_origin_vectors %>%
  dplyr::mutate(
    search_z_cut = age_groups_limits[cut(search_z, age_groups_limits, labels = F)] + 500
  ) %>%
  dplyr::select(region_id, search_z_cut, ov_angle_deg, ov_dist) %>%
  dplyr::mutate(
    angle_deg_cut = as.numeric(as.character(
      cut(ov_angle_deg, breaks = seq(0,360,45), labels = seq(22.5,360,45))
    ))
  ) %>%
  dplyr::filter(search_z_cut %in% seq(-6000, 1000, 1000)) %>%
  dplyr::mutate(
    z_named = dplyr::recode_factor(as.character(search_z_cut), !!!rev(list(
      "-6000" = "6500-5500 calBC", 
      "-5000" = "5500-4500 calBC",
      "-4000" = "4500-3500 calBC",
      "-3000" = "3500-2500 calBC",
      "-2000" = "2500-1500 calBC",
      "-1000" = "1500-500 calBC",
      "0" = "500 calBC - 500 calAD",
      "1000"  = "500-1500 calAD"
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
    size = 0.1,
    color = "grey"
  ) +
  geom_segment(
    data = data.frame(deg = seq(0,315,45)),
    mapping = aes(x = deg, xend = deg),
    y = 0,
    yend = 3000,
    size = 0.1,
    color = "grey"
  ) +
  geom_boxplot(
    aes(angle_deg_cut, ov_dist, group = angle_deg_cut), 
    width = 45,
    size = 0.3,
    outlier.size = 0.4,
    outlier.color = "red",
    fill = "#fcbab6",
    color = "red"
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
  ylab("spatial distance to \"origin point\" in km") +
  xlab(NULL) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    panel.border = element_blank()
  )
  
ggsave(
  paste0("plots/figure_sup_12_direction_windrose_matrix.pdf"),
  plot = p,
  device = "pdf",
  scale = 1,
  dpi = 300,
  width = 240, height = 300, units = "mm",
  limitsize = F
)
