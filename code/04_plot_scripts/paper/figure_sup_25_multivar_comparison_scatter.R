library(magrittr)
library(ggplot2)

load("data/genotype_data/janno_final.RData")
load("data/plot_reference_data/region_id_shapes.RData")
load("data/plot_reference_data/age_colors_gradient.RData")

p_multivar <- function(v1, v2, plot_order_dim, rev_x = F, rev_y = F) {
  p0 <- ggplot() +
    geom_point(
      data = janno_final %>% dplyr::arrange(.data[[plot_order_dim]]),
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
    theme(legend.position = "none")
  p1 <- if (rev_x) { p0 + scale_x_reverse() } else { p0 }
  p2 <- if (rev_y) { p1 + scale_y_reverse() } else { p1 }
  return(p2)
}

p_pca_u <- p_multivar("C1_pca_u", "C2_pca_u", "C3_pca_u", F, T)
p_pca_f <- p_multivar("C1_pca_f", "C2_pca_f", "C3_pca_f", F, T)
p_pca_proj_u <- p_multivar("C1_pca_proj_u", "C2_pca_proj_u", "C3_pca_proj_u", F, T)
p_pca_proj_f <- p_multivar("C1_pca_proj_f", "C2_pca_proj_f", "C3_pca_proj_f", F, T)
p_mds_u <- p_multivar("C1_mds_u", "C2_mds_u", "C3_mds_u")
p_mds_f <- p_multivar("C1_mds_f", "C2_mds_f", "C3_mds_f")
p_emu_u <- p_multivar("C1_emu_u", "C2_emu_u", "C3_emu_u", T, F)
p_emu_f <- p_multivar("C1_emu_f", "C2_emu_f", "C3_emu_f", T, F)


p_legend <- cowplot::get_legend(
  p_emu_f +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.background = element_blank(),
    legend.title = element_text(size = 13),
    legend.spacing.y = unit(0.2, 'cm'),
    legend.key.height = unit(0.4, 'cm'),
    legend.text = element_text(size = 10),
  ) +
  guides(
    color = guide_colorbar(title = "Time", barwidth = 1.5, barheight = 20),
    shape = guide_legend(
      title = "Region", ncol = 1, byrow = T,
      override.aes = aes(size = 3, stroke = 1)
    )
  )
)

p_left <- cowplot::plot_grid(
  cowplot::plot_grid(p_pca_u, p_pca_f, ncol = 2, labels = c("A", "B")),
  cowplot::plot_grid(p_pca_proj_u, p_pca_proj_f, ncol = 2, labels = c("C", "D")),
  cowplot::plot_grid(p_mds_u, p_mds_f, ncol = 2, labels = c("E", "F")),
  cowplot::plot_grid(p_emu_u, p_emu_f, ncol = 2, labels = c("G", "H")),
  nrow = 4, rel_heights = c(1,1,1,1)#,0.635)
)

p_total <- cowplot::plot_grid(
  p_left,
  p_legend,
  rel_widths = c(3,1)
)

ggsave(
  paste0("plots/figure_sup_25_multivar_comparison_scatter.pdf"),
  plot = p_total,
  device = "pdf",
  scale = 1.2,
  dpi = 300,
  width = 220, height = 300, units = "mm",
  limitsize = F,
  bg = "white"
)
