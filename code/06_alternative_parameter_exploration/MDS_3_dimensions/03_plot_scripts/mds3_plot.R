library(magrittr)

load("data/poseidon_data/janno_final.RData")
load("data/plot_reference_data/region_id_shapes.RData")
load("data/plot_reference_data/age_colors_gradient.RData")

# mean per region and time
region_age_group_mean <- janno_final %>%
  dplyr::filter(!is.na(region_id)) %>%
  dplyr::group_by(region_id, age_group_id) %>%
  dplyr::summarise(
    mds3_C1 = mean(mds3_C1),
    mds3_C2 = mean(mds3_C2),
    mds3_C3 = mean(mds3_C3),
    z = mean(Date_BC_AD_Median_Derived)
  ) %>%
  dplyr::ungroup()

p_mds <- function(v1, v2, plot_order_dim, grid_x, grid_y) {
  ggplot() +
    geom_point(
      data = janno_final %>% dplyr::arrange(.data[[plot_order_dim]]),
      aes(
        x = .data[[v1]], y = .data[[v2]], 
        color = Date_BC_AD_Median_Derived,
        shape = region_id
      ),
      size = 2
    ) +
    # ggpointgrid::geom_pointgrid(
    #   data = region_age_group_mean,
    #   aes(x = .data[[v1]], y = .data[[v2]]),
    #   size = 5,
    #   fill = "black",
    #   color = "black",
    #   shape = 21,
    #   grid_x = grid_x,
    #   grid_y = grid_y
    # ) +
    # ggpointgrid::geom_pointgrid(
    #   data = region_age_group_mean,
    #   aes(x = .data[[v1]], y = .data[[v2]], color = z, shape = region_id),
    #   size = 2,
    #   grid_x = grid_x,
    #   grid_y = grid_y,
    #   stroke = 1
    # ) +
    scale_shape_manual(
      values = region_id_shapes,
      na.value = 3
    ) +
    age_colors_gradient +
    coord_fixed() +
    scale_y_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
    scale_x_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.background = element_blank(),
      legend.title = element_text(size = 13),
      legend.spacing.y = unit(0.2, 'cm'),
      legend.key.height = unit(0.4, 'cm'),
      legend.text = element_text(size = 10),
    ) +
    guides(
      color = guide_colorbar(title = "Time", barwidth = 20, barheight = 1.5),
      shape = guide_legend(
        title = "Region", nrow = 3, ncol = 3, byrow = T,
        override.aes = aes(size = 3, stroke = 1)
      )
    )
}

p_C1C2 <- p_mds("mds3_C1", "mds3_C2", "mds3_C3", 20, 25)
p_legend <- cowplot::get_legend(p_C1C2)

p_C1C2 <- p_C1C2 + theme(legend.position = "none")
p_C1C3 <- p_mds("mds3_C1", "mds3_C3", "mds3_C2", 20, 10) + theme(legend.position = "none")
p_C3C2 <- p_mds("mds3_C3", "mds3_C2", "mds3_C1", 10, 25) + theme(legend.position = "none") + scale_x_reverse()

total <- cowplot::plot_grid(
  cowplot::plot_grid(p_C1C2, p_C3C2, ncol = 2),
  cowplot::plot_grid(p_C1C3, p_legend, ncol = 2), 
  nrow = 2, rel_heights = c(1,0.635)
)

ggsave(
  paste0("plots/figure_2_mds3.jpeg"),
  plot = total,
  device = "jpeg",
  scale = 0.9,
  dpi = 300,
  width = 320, height = 300, units = "mm",
  limitsize = F,
  bg = "white"
)

