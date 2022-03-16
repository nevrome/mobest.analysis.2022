library(magrittr)
library(ggplot2)

load("data/genotype_data/janno_final.RData")
load("data/plot_reference_data/region_id_shapes.RData")
load("data/plot_reference_data/age_colors_gradient.RData")

p_mds <- function(v1, v2, plot_order_dim, plot_order_factor = 1) {
  ggplot() +
    geom_point(
      data = janno_final %>% dplyr::arrange(.data[[plot_order_dim]] * plot_order_factor),
      aes(
        x = .data[[v1]], y = .data[[v2]], 
        color = Date_BC_AD_Median_Derived,
        shape = region_id
      ),
      size = 2
    ) +
    scale_shape_manual(
      values = region_id_shapes
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

p_C1C2 <- p_mds("C1", "C2", "C3")
p_legend <- cowplot::get_legend(p_C1C2)

p_C1C2 <- p_C1C2 + theme(legend.position = "none")
p_C1C3 <- p_mds("C1", "C3", "C2", -1) + theme(legend.position = "none")
p_C3C2 <- p_mds("C3", "C2", "C1") + theme(legend.position = "none") + scale_x_reverse()

total <- cowplot::plot_grid(
  cowplot::plot_grid(p_C1C2, p_C3C2, ncol = 2, labels = c("A", "B")),
  cowplot::plot_grid(p_C1C3, p_legend, ncol = 2, labels = c("C", NA)), 
  nrow = 2, rel_heights = c(1,0.635)
)

ggsave(
  paste0("plots/figure_sup_14_mds3.pdf"),
  plot = total,
  device = "pdf",
  scale = 0.9,
  dpi = 300,
  width = 320, height = 300, units = "mm",
  limitsize = F,
  bg = "white"
)
