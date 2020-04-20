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

p <- ggplot() +
  geom_point(
    data = pca_ref,
    aes(x = PC1, y = PC2),
    shape = 20,
    size = 0.2
  ) +
  ggnewscale::new_scale_color() +
  geom_point(
    data = anno,
    aes(x = PC1, y = PC2, color = region_id, shape = age_group_id),
    alpha = 0.7,
    size = 2
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "right"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  scale_shape_manual(
    values = c(
      ">-10000" = 15,
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
  coord_fixed()

ggsave(
  paste0("plots/samples_PCA.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.8,
  dpi = 300,
  width = 330, height = 250, units = "mm",
  limitsize = F
)

