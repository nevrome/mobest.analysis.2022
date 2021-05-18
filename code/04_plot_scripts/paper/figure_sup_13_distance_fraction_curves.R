library(ggplot2)
library(magrittr)

load("data/poseidon_data/janno_final.RData")
load("data/origin_search/moving_origin_grid.RData")

moving_origin_grid_modified <- moving_origin_grid %>%
  dplyr::mutate(
    # ugly workaround for geom_ribbon, because geom_area can't deal well with 
    # NA values
    fraction_bigger_2000,
    fraction_bigger_1000 = fraction_bigger_2000 + fraction_bigger_1000,
    fraction_bigger_500  = fraction_bigger_1000 + fraction_bigger_500,
    fraction_smaller_500 = fraction_bigger_500 + fraction_smaller_500
  ) %>%
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
        "fraction_bigger_1000",
        "fraction_bigger_2000"
      )
    )
  )

p_curves <- moving_origin_grid_modified %>%
  ggplot() +
  lemon::facet_rep_wrap(~region_id, ncol = 2, repeat.tick.labels = T) +
  geom_ribbon(
    aes(
      x = z, 
      ymin = 0,
      ymax = number, 
      fill = fraction_type
    ),
    na.rm = F
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
      "Distances between 1000 and 2000km",
      "Distances bigger then 2000km"
    ),
    start = 0.7,
    end = 0.1
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
