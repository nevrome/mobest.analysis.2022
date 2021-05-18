library(ggplot2)
library(magrittr)

load("data/poseidon_data/janno_final.RData")
load("data/origin_search/moving_origin_grid.RData")

moving_origin_grid_modified <- moving_origin_grid %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("fraction"),
    names_to = "fraction_type",
    values_to = "number"
  ) %>%
  dplyr::mutate(
    fraction_type = factor(
      fraction_type, levels = c(
        "fraction_smaller_500",
        "fraction_bigger_500",
        "fraction_bigger_1000"
      )
    )
  )

p_curves <- moving_origin_grid_modified %>%
  ggplot() +
  lemon::facet_rep_wrap(~region_id, ncol = 2, repeat.tick.labels = T) +
  geom_area(
    aes(x = z, y = number, fill = fraction_type)
  ) +
  geom_point(
    data = janno_final %>% dplyr::filter(!is.na(region_id)),
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
      "Distances bigger then 1000km"
    ),
    start = 0.7,
    end = 0.3
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(-7000, 1000, 1000)) +
  coord_cartesian(
    xlim = c(-7400, 1400)
  )

p_legend <- cowplot::get_legend((p_curves + theme(legend.position = "right")))

p <- cowplot::ggdraw(p_curves) +
  cowplot::draw_plot(
    p_legend,
    x = 0.65, y = 0.05, 
    width = 0.22, height = 0.22
  )

ggsave(
  paste0("plots/figure_sup_13_distance_fraction_curves.png"),
  plot = p,
  device = "png",
  scale = 0.7,
  dpi = 300,
  width = 400, height = 300, units = "mm",
  limitsize = F
)
