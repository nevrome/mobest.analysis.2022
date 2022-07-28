library(magrittr)
library(ggplot2)

load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
load("data/origin_search/janno_search.RData")
load("data/origin_search/location_examples.RData")

p_loc <- function(loc, loc_name) {
  ggplot() +
    facet_wrap(~search_id, ncol = 2) +
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
      legend.box = "horizontal",
      legend.title = element_text(size = 15),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.text = element_text(size = 15),
      strip.text = element_text(size = 15),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "#BFD5E3")
    ) +
    ggtitle(loc_name)
}

p_C1_pca <- location_examples %>% dplyr::filter(dependent_var_id == "C1_pca_proj_u") %>% p_loc("PCA C1")
p_C2_pca <- location_examples %>% dplyr::filter(dependent_var_id == "C2_pca_proj_u") %>% p_loc("PCA C2")
p_C1toC2_pca <- location_examples_C1toC2_pca_proj_u %>% p_loc("PCA C1*C2")
p_C3_pca <- location_examples %>% dplyr::filter(dependent_var_id == "C3_pca_proj_u") %>% p_loc("PCA C3")
p_C1toC3_pca <- location_examples_C1toC3_pca_proj_u %>% p_loc("PCA C1*C2*C3")
p_C4_pca <- location_examples %>% dplyr::filter(dependent_var_id == "C3_pca_proj_u") %>% p_loc("PCA C4")
p_C1toC4_pca <- location_examples_C1toC4_pca_proj_u %>% p_loc("PCA C1*C2*C3*C4")
p_C5_pca <- location_examples %>% dplyr::filter(dependent_var_id == "C3_pca_proj_u") %>% p_loc("PCA C5")
p_C1toC5_pca <- location_examples_C1toC5_pca_proj_u %>% p_loc("PCA C1*C2*C3*C4*C5")

p <- cowplot::plot_grid(
  p_C1_pca, NULL,
  p_C2_pca, p_C1toC2_pca,
  p_C3_pca, p_C1toC3_pca,
  p_C4_pca, p_C1toC4_pca,
  p_C5_pca, p_C1toC5_pca,
  labels = "AUTO", ncol = 2
)

ggsave(
  "plots/figure_sup_32_genetic_distance_example_maps_large.pdf",
  plot = p,
  device = "pdf",
  scale = 0.8,
  dpi = 300,
  width = 300, height = 750, units = "mm",
  limitsize = F
)
