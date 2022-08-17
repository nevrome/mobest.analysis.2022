library(magrittr)
library(ggplot2)

load("data/genotype_data/janno_final.RData")
load("data/plot_reference_data/region_id_shapes.RData")
load("data/plot_reference_data/age_colors_gradient.RData")

p_pca <- function(v1, v2, plot_order_dim, plot_order_factor = 1) {
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
    #coord_fixed() +
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

C1 <- "C1_pca_proj_u"
C2 <- "C2_pca_proj_u"
C3 <- "C3_pca_proj_u"
C4 <- "C4_pca_proj_u"
C5 <- "C5_pca_proj_u"

p_C1C2 <- p_pca(C1, C2, C3)
p_legend <- cowplot::get_legend(p_C1C2)

p_C1C2 <- p_C1C2 + theme(legend.position = "none")
p_C1C3 <- p_pca(C1, C3, C2, -1) + theme(legend.position = "none")
p_C3C2 <- p_pca(C3, C2, C1) + theme(legend.position = "none") + scale_x_reverse()

p_C1C4 <- p_pca(C1, C4, C1, -1) + theme(legend.position = "none")
p_C1C5 <- p_pca(C1, C5, C1, -1) + theme(legend.position = "none")

total <- cowplot::plot_grid(
  p_C1C2, p_C3C2, p_C1C3, p_legend, p_C1C4, p_C1C5,
  nrow = 3, ncol = 2, labels = c("A", "B", "C", NA, "D", "E"),
  rel_heights = c(1,1,1), align = "hv", axis = "l"
)

ggsave(
  paste0("plots/figure_sup_14_pca_proj.pdf"),
  plot = total,
  device = "pdf",
  scale = 0.9,
  dpi = 300,
  width = 320, height = 400, units = "mm",
  limitsize = F,
  bg = "white"
)
