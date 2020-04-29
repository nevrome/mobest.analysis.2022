library(magrittr)
library(ggplot2)

mds_unfiltered <- readr::read_delim("data/mds/1240K_HumanOrigins.pruned.mds", " ", trim_ws = T) %>%
  dplyr::select(-FID, -X8, -SOL)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final

anno <- anno %>%
  dplyr::left_join(
    mds_unfiltered, by = c("sample_id" = "IID"), suffix = c("", "_unfiltered")
  )

p <- ggplot() +
  geom_point(
    data = anno,
    aes(x = C1_unfiltered, y = C2_unfiltered, color = region_id, shape = age_group_id),
    alpha = 0.7,
    size = 2
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
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
    color = guide_legend(title = ""),
    shape = guide_legend(title = "median age calBC")
  ) +
  #coord_fixed() +
  scale_y_reverse()

ggsave(
  paste0("plots/samples_MDS_unfiltered.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.5,
  dpi = 300,
  width = 400, height = 230, units = "mm",
  limitsize = F
)
