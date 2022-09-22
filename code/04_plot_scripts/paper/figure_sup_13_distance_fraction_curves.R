library(ggplot2)
library(magrittr)

load("data/genotype_data/janno_final.RData")
load("data/origin_search/origin_summary.RData")

origin_summary %<>%
  dplyr::filter(multivar_method == "mds2", search_time == -667) %>%
  dplyr::filter(region_id != "Other region")

origin_summary_unnested <- origin_summary %>%
  dplyr::mutate(., moving_window = 1:nrow(.)) %>%
  tidyr::unnest(cols = ov_dist_fractions, keep_empty = T) %>%
  dplyr::select(-count) %>%
  dplyr::mutate(
    ov_dist_fraction_label = dplyr::case_when(
      ov_dist_length_lower_end == 0 ~ "Distances smaller then 500km",
      ov_dist_length_lower_end == 500 ~ "Distances between 500 and 1000km",
      ov_dist_length_lower_end == 1000 ~ "Distances between 1000 and 2000km",
      ov_dist_length_lower_end == 2000 ~ "Distances bigger than 2000km"
    )
  )

p <- origin_summary_unnested %>%
  ggplot() +
  lemon::facet_rep_wrap(~region_id, ncol = 2, repeat.tick.labels = T) +
  geom_area(
    aes(
      x = z, 
      y = fraction,
      fill = as.factor(ov_dist_length_lower_end)
    ),
    color = "lightgrey", size = 0.1
  ) +
  geom_point(
    data = janno_final %>% dplyr::filter(region_id != "Other region"),
    aes(x = Date_BC_AD_Median_Derived, y = -0.04),
    shape = "|"
  ) +
  xlab("time in years calBC/calAD") +
  ylab("Distance class fractions") +
  scale_fill_grey(
    name = "Distance classes",
    labels = c(
      "Distances smaller then 500km",
      "Distances between 500 and 1000km",
      "Distances between 1000 and 2000km",
      "Distances bigger than 2000km"
    ),
    start = 0.7,
    end = 0.1
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(-7000, 1000, 1000)) +
  coord_cartesian(
    xlim = c(-7400, 1400)
  )

ggsave(
  paste0("plots/figure_sup_13_distance_fraction_curves.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 430, height = 300, units = "mm",
  limitsize = F
)
