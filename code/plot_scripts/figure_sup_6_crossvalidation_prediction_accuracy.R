library(magrittr)
library(ggplot2)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final
load("data/parameter_exploration/crossvalidation/interpol_comparison.RData")

sample_interpol_comparison <- interpol_comparison %>%
  dplyr::sample_n(10000)

sample_interpol_comparison$difference[sample_interpol_comparison$dependent_var == "PC1_dist"] %<>% `/`(dist(range(anno$PC1)))
sample_interpol_comparison$difference[sample_interpol_comparison$dependent_var == "PC2_dist"] %<>% `/`(dist(range(anno$PC2)))
sample_interpol_comparison$difference[sample_interpol_comparison$dependent_var == "C1_dist"] %<>% `/`(dist(range(anno$C1, na.rm = T)))
sample_interpol_comparison$difference[sample_interpol_comparison$dependent_var == "C2_dist"] %<>% `/`(dist(range(anno$C2, na.rm = T)))

p <- sample_interpol_comparison %>%
  ggplot(aes(y = dependent_var, x = difference, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  ggridges::stat_density_ridges(
    geom = "density_ridges_gradient", 
    calc_ecdf = TRUE
  ) +
  viridis::scale_fill_viridis(name = "Tail probability") +
  geom_vline(
    mapping = aes(xintercept = 0),
    color = "red"
  ) +
  theme_bw() +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0.5))) +
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.1)) +
  xlab("Normalitzed difference between prediction and measured ancestry component") +
  ylab("Density curve for each ancestry component")

ggsave(
  "plots/figure_sup_6_crossvalidation_prediction_accuracy.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 150, units = "mm",
  limitsize = F
)
