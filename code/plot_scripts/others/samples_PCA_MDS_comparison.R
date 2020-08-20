library(magrittr)
library(ggplot2)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_pca.RData")
ref_pops <- readLines("data/population_lists/PCA_6.pops")

pca_ref <- anno_1240K_and_anno_1240K_HumanOrigins_pca %>%
  dplyr::filter(
    group_label %in% ref_pops
  )

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final

p1 <- ggplot() +
  geom_point(
    data = pca_ref,
    aes(x = PC1, y = PC2),
    shape = 20,
    size = 0.2
  ) +
  geom_point(
    data = anno,
    aes(x = PC1, y = PC2, color = region_id, shape = age_group_id),
    alpha = 0.7,
    size = 2
  ) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  scale_shape_manual(
    values = c(
      ">-8000" = 15,
      "-8000 - -6000" = 15,
      "-6000 - -4000" = 17,
      "-4000 - -2000" = 6,
      "-2000 - 0" = 4
    )
  ) +
  scale_color_manual(
    values = c(
      "#999999", "#E69F00", "#56B4E9", "#009E73", "#871200",
      "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#2fff00"
    )
  ) +
  guides(
    color = FALSE,
    shape = FALSE
  ) +
  coord_fixed()

p2 <- ggplot() +
  geom_point(
    data = anno,
    aes(x = C1, y = C2, color = region_id, shape = age_group_id),
    alpha = 0.7,
    size = 2
  ) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "right"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  scale_shape_manual(
    values = c(
      ">-8000" = 15,
      "-8000 - -6000" = 15,
      "-6000 - -4000" = 17,
      "-4000 - -2000" = 6,
      "-2000 - 0" = 4
    )
  ) +
  scale_color_manual(
    values = c(
      "#999999", "#E69F00", "#56B4E9", "#009E73", "#871200",
      "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#2fff00"
    )
  ) +
  guides(
    color = FALSE,
    shape = FALSE
  ) +
  coord_fixed() +
  scale_y_reverse()

p <- cowplot::plot_grid(p1, p2, nrow = 1)

ggsave(
  paste0("plots/samples_PCA_MDS_comparison.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.5,
  dpi = 300,
  width = 400, height = 230, units = "mm",
  limitsize = F
)
