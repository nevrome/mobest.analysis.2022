library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")
load("data/plot_reference_data/region_id_shapes.RData")
load("data/plot_reference_data/age_colors_gradient.RData")
source("code/04_plot_scripts/paper/individuals_to_highlight.R")

lookup <- individuals %>% 
  dplyr::inner_join(janno_final, by = c("search_id" = "Individual_ID"))

lookup_top          <- lookup %>% dplyr::filter(C2 > -0.01)
lookup_left         <- lookup_top %>% dplyr::filter(C1 < mean(C1))
lookup_right        <- lookup_top %>% dplyr::filter(C1 >= mean(C1))
lookup_right_top    <- lookup_right %>% dplyr::filter(C2 > 0.015)
lookup_right_bottom <- lookup_right %>% dplyr::filter(C2 <= 0.015)
lookup_bottom       <- lookup %>% dplyr::filter(C2 <= -0.01)

repel <- function(data, direction, nudge_y = 0, nudge_x = 0) {
  ggrepel::geom_text_repel(
    data = data,
    mapping = aes(x = C1, y = C2, label = label_name),
    force_pull = 0,
    nudge_y = nudge_y,
    nudge_x = nudge_x,
    direction = direction,
    segment.size = 0.3,
    arrow = arrow(length = unit(0.005, "npc")),
    min.segment.length = unit(0.02, "npc"),
    point.padding = 0.5,
    size = 3
  )
}

p <- ggplot() +
  geom_point(
    data = janno_final,
    aes(
      x = C1, y = C2, 
      color = Date_BC_AD_Median_Derived,
      shape = region_id
    ),
    size = 2
  ) +
  geom_point(
    data = lookup,
    aes(x = C1, y = C2),
    size = 4, shape = 21, fill = "white", alpha = 0.8, stroke = 0
  ) +
  geom_point(
    data = lookup,
    aes(x = C1, y = C2, shape = region_id),
    size = 2, colour = "black"
  ) +
  repel(lookup_right_top, direction = "x", nudge_y = 0.06 - lookup_right_top$C2) +
  repel(lookup_right_bottom, direction = "y", nudge_x = 0.06 - lookup_right_bottom$C1) +
  repel(lookup_left, direction = "y", nudge_x = -0.082 - lookup_left$C1) +
  repel(lookup_bottom, direction = "x", nudge_y = -0.062 - lookup_bottom$C2) +
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

ggsave(
  paste0("plots/figure_sup_23_mds_with_highlighted_individuals.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 1,
  dpi = 300,
  width = 220, height = 300, units = "mm",
  limitsize = F
)
