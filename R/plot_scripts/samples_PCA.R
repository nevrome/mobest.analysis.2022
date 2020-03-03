library(magrittr)
library(ggplot2)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_pca.RData")
ref_pops <- readLines("data/population_lists/PCA_6.pops")
ref_pops_grouping <- readr::read_csv("data_tracked/reference_population_grouping/PCA_6_grouped", col_names = F)

pca_ref <- anno_1240K_and_anno_1240K_HumanOrigins_pca %>%
  dplyr::filter(
    group_label %in% ref_pops
  ) %>%
  dplyr::left_join(
    ref_pops_grouping, by = c("group_label" = "X1")
  )

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered

p <- ggplot() +
  geom_point(
    data = pca_ref,
    aes(x = PC1, y = PC2, color = X2)
  ) +
  geom_point(
    data = anno,
    aes(x = PC1, y = PC2),
    color = "black",
    alpha = 0.3
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20)
  )

ggsave(
  paste0("plots/samples_PCA.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.8,
  dpi = 300,
  width = 300, height = 250, units = "mm",
  limitsize = F
)

