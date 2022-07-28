library(magrittr)
library(ggplot2)

load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
load("data/origin_search/janno_search.RData")
load("data/origin_search/location_examples.RData")

p_loc <- function(loc, loc_name, show_dots = F) {
  p <- ggplot() +
    facet_wrap(
      ~search_id, ncol = 3,
      labeller = ggplot2::labeller(search_id = c(
       "Stuttgart_published.DG" = "Stuttgart ~5250BC (-1500y)",
       "RISE434.SG" = "RISE434 ~2750BC (-300y)",
       "3DT26.SG" = "3DRIF-26 ~200AD (0y)",
       "SI-40.SG" = "SI-40 ~1150AD (0y)",
       "I8341" = "I8341 ~400BC (-100y)",
       "I8215" = "I8215 ~550BC (-100y)"
      ))) +
    geom_sf(data = extended_area, fill = "black") +
    geom_raster(
      data = loc,
      mapping = aes(x = field_x, y = field_y, fill = probability),
    ) +
    scale_fill_viridis_c(option = "mako", direction = -1) +
    geom_sf(data = extended_area, fill = NA, colour = "black") +
    geom_point(
      data = janno_search,
      mapping = aes(x = x, y = y),
      fill = "red", colour = "black", shape = 21,
      size = 3
    ) +
    theme_bw() +
    coord_sf(
      expand = FALSE,
      crs = epsg3035
    ) +
    guides(
      fill = guide_colorbar(title = "Probability  ", barwidth = 25)
    ) +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.text = element_blank(),
      strip.text = element_text(size = 15),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "#BFD5E3"),
      title = element_text(size = 15, face = "bold")
    ) +
    ggtitle(loc_name)
  # add search dots
  if (show_dots) {
    p <- p + 
        geom_point(
        data = loc %>% 
          dplyr::group_by(search_id) %>%
          dplyr::slice_max(probability) %>%
          dplyr::ungroup(),
        mapping = aes(x = field_x, y = field_y),
        fill = "orange", colour = "black", shape = 21,
        size = 3
      )
  }
  # hack to colour the facet strips
  g <- ggplot_gtable(ggplot_build(p))
  stripr <- which(grepl('strip-', g$layout$name))
  fills <- c("#B40F20", "white", "#FDD262", "#5BBCD6", "#00A08A", "#F98400")
  k <- 1
  for (i in stripr) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  #grid::grid.draw(g)
  ggpubr::as_ggplot(g)
}

p_C1_pca <- location_examples %>% dplyr::filter(dependent_var_id == "C1_pca_proj_u") %>% p_loc("PCA C1")
p_C2_pca <- location_examples %>% dplyr::filter(dependent_var_id == "C2_pca_proj_u") %>% p_loc("PCA C2")
p_C1toC2_pca <- location_examples_C1toC2_pca_proj_u %>% p_loc("PCA C1*C2", T)
p_C3_pca <- location_examples %>% dplyr::filter(dependent_var_id == "C3_pca_proj_u") %>% p_loc("PCA C3")
p_C1toC3_pca <- location_examples_C1toC3_pca_proj_u %>% p_loc("PCA C1*C2*C3", T)

p_C4_pca <- location_examples %>% dplyr::filter(dependent_var_id == "C4_pca_proj_u") %>% p_loc("PCA C4")
p_C1toC4_pca <- location_examples_C1toC4_pca_proj_u %>% p_loc("PCA C1*C2*C3*C4", T)
p_C5_pca <- location_examples %>% dplyr::filter(dependent_var_id == "C5_pca_proj_u") %>% p_loc("PCA C5")
p_C1toC5_pca <- location_examples_C1toC5_pca_proj_u %>% p_loc("PCA C1*C2*C3*C4*C5", T)
p_C6_pca <- location_examples %>% dplyr::filter(dependent_var_id == "C6_pca_proj_u") %>% p_loc("PCA C6")
p_C1toC6_pca <- location_examples_C1toC6_pca_proj_u %>% p_loc("PCA C1*C2*C3*C4*C5*C6", T)

p1 <- cowplot::plot_grid(
  p_C1_pca, NULL,
  p_C2_pca, p_C1toC2_pca,
  p_C3_pca, p_C1toC3_pca,
  labels = NULL, ncol = 2
)

p2 <- cowplot::plot_grid(
  p_C4_pca, p_C1toC4_pca,
  p_C5_pca, p_C1toC5_pca,
  p_C6_pca, p_C1toC6_pca,
  labels = NULL, ncol = 2
)

ggsave(
  "plots/figure_sup_32_1_genetic_distance_example_maps_large.pdf",
  plot = p1,
  device = "pdf",
  scale = 1.2,
  dpi = 300,
  width = 400, height = 350, units = "mm",
  limitsize = F
)

ggsave(
  "plots/figure_sup_32_2_genetic_distance_example_maps_large.pdf",
  plot = p2,
  device = "pdf",
  scale = 1.2,
  dpi = 300,
  width = 400, height = 350, units = "mm",
  limitsize = F
)
