library(magrittr)
library(ggplot2)

load("data/genotype_data/janno_final.RData")
load("data/plot_reference_data/region_id_shapes.RData")
load("data/plot_reference_data/age_colors_gradient.RData")

# normal mds plot
p <- ggplot() +
  geom_point(
    data = janno_final,
    aes(
      x = C1_mds_f, y = C3_mds_f, 
      color = Date_BC_AD_Median_Derived,#dplyr::case_when(grepl(".SG", Poseidon_ID) ~ "Shotgun", TRUE ~ "Capture"), #,
      shape = region_id,
      label = purrr::map_chr(Group_Name, \(x) x[[1]])
    ),
    size = 2
  ) +
  scale_shape_manual(
    values = region_id_shapes,
    na.value = 3
  ) +
  age_colors_gradient +
  coord_fixed() +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.box = "vertical",
    legend.background = element_blank(),
    legend.title = element_text(size = 13),
    legend.spacing.y = unit(0.2, 'cm'),
    legend.key.height = unit(0.4, 'cm'),
    legend.text = element_text(size = 10),
  )

ggsave(
  paste0("plots/pres_figure.png"),
  plot = p,
  device = "png",
  scale = 0.7,
  dpi = 300,
  width = 220, height = 150, units = "mm",
  limitsize = F
)

