library(magrittr)
library(ggplot2)

load("data/genotype_data/janno_final.RData")

interpol_comparison_C1to3_mds_u <- lapply(
  list.files(
    "data/parameter_exploration/crossvalidation", 
    pattern = "^interpol_comparison_C[1-3]{1}_mds_u",
    full.names = T
  ), function(x) {
    load(x)
    interpol_comparison
  }
) %>% dplyr::bind_rows()

sample_interpol_comparison <- interpol_comparison_C1to3_mds_u %>%
  dplyr::sample_n(200000)

sample_interpol_comparison$difference[sample_interpol_comparison$dependent_var_id == "C1_mds_u"] %<>% 
  `/`(dist(range(janno_final$C1_mds_u, na.rm = T)))
sample_interpol_comparison$difference[sample_interpol_comparison$dependent_var_id == "C2_mds_u"] %<>% 
  `/`(dist(range(janno_final$C2_mds_u, na.rm = T)))
sample_interpol_comparison$difference[sample_interpol_comparison$dependent_var_id == "C3_mds_u"] %<>% 
  `/`(dist(range(janno_final$C3_mds_u, na.rm = T)))

p <- sample_interpol_comparison %>%
  ggplot(aes(y = dependent_var_id, x = difference, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
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
  scale_y_discrete(limits = rev, expand = expansion(mult = c(0.1, 0.1), add = c(0.1, 1))) +
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.1)) +
  xlab("Normalized difference between predicted and measured ancestry") +
  ylab("Density curves")

ggsave(
  "plots/figure_sup_8_crossvalidation_prediction_accuracy.pdf",
  plot = p,
  device = "pdf",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 120, units = "mm",
  limitsize = F
)
