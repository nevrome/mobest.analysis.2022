library(magrittr)
library(ggplot2)

load("data/genotype_data/janno_final.RData")

janno_final$shotgun_capture <- ifelse(grepl(".SG", janno_final$Poseidon_ID), "Shotgun", "Capture")

p_multivar <- function(v1, v2, rev_x = F, rev_y = F) {
  p0 <- ggplot() +
    geom_point(
      data = janno_final,
      aes(
        x = .data[[v1]], y = .data[[v2]], 
        color = shotgun_capture,
      ),
      size = 1
    ) +
    #coord_fixed() +
    theme_bw() +
    theme(legend.position = "none")
  p1 <- if (rev_x) { p0 + scale_x_reverse() } else { p0 }
  p2 <- if (rev_y) { p1 + scale_y_reverse() } else { p1 }
  return(p2)
}

p_emu_u <- p_multivar("C1_emu_u", "C3_emu_u", T, F)
p_emu_f <- p_multivar("C1_emu_f", "C3_emu_f", T, F)
p_mds_u <- p_multivar("C1_mds_u", "C3_mds_u")
p_mds_f <- p_multivar("C1_mds_f", "C3_mds_f")

p_legend <- cowplot::get_legend(
  p_emu_f +
    theme(
      legend.position = "bottom"
    ) +
    guides(color = guide_legend(title = "Data preparation"))
)

p_total <- cowplot::plot_grid(
  cowplot::plot_grid(p_emu_u, p_emu_f, ncol = 2, labels = c("A", "B")),
  cowplot::plot_grid(p_mds_u, p_mds_f, ncol = 2, labels = c("C", "D")),
  p_legend,
  nrow = 3, rel_heights = c(1,1,0.1)#,0.635)
)

ggsave(
  paste0("plots/figure_sup_26_multivar_comparison_capture_shotgun_mds_emu.pdf"),
  plot = p_total,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 300, height = 300, units = "mm",
  limitsize = F,
  bg = "white"
)

