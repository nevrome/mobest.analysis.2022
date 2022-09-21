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
  dplyr::select(-count)

fillibus <- expand.grid(
  ov_dist_length_upper_end = seq(500, 4000, 500),
  moving_window = origin_summary_unnested$moving_window
) %>% tibble::as_tibble()

fillibuster <- dplyr::anti_join(
  fillibus,
  origin_summary_unnested,
  by = c("moving_window", "ov_dist_length_upper_end")
)

origin_summary_unnested %>%
  dplyr::left_join(
    fillibuster,
    by = c("moving_window")
  ) %>% View()



  # dplyr::mutate(
  #   # ugly workaround for geom_ribbon, because geom_area can't deal well with 
  #   # NA values
  #   fraction_bigger_2000,
  #   fraction_bigger_1000 = fraction_bigger_2000 + fraction_bigger_1000,
  #   fraction_bigger_500  = fraction_bigger_1000 + fraction_bigger_500,
  #   fraction_smaller_500 = fraction_bigger_500 + fraction_smaller_500
  # ) %>%
  # tidyr::pivot_longer(
  #   cols = tidyselect::starts_with("fraction"),
  #   names_to = "fraction_type",
  #   values_to = "number"
  # ) %>%
  # dplyr::mutate(
  #   fraction_type = factor(
  #     fraction_type, levels = c(
  #       "fraction_smaller_500",
  #       "fraction_bigger_500",
  #       "fraction_bigger_1000",
  #       "fraction_bigger_2000"
  #     )
  #   )
  # )

p <- moving_origin_grid_modified %>%
  ggplot() +
  lemon::facet_rep_wrap(~region_id, ncol = 2, repeat.tick.labels = T) +
  geom_ribbon(
    aes(
      x = z, 
      ymin = 0,
      ymax = fraction, 
      fill = as.factor(ov_dist_length_upper_end)
    ),
    na.rm = F
  ) +
  geom_point(
    data = janno_final %>% dplyr::filter(region_id != "Other region"),
    aes(x = Date_BC_AD_Median_Derived, y = -0.04),
    shape = "|"
  ) +
  xlab("time in years calBC/calAD") +
  ylab("Distance class fractions") +
  # scale_fill_grey(
  #   name = "Distance classes",
  #   labels = c(
  #     "Distances smaller then 500km",
  #     "Distances between 500 and 1000km",
  #     "Distances between 1000 and 2000km",
  #     "Distances bigger than 2000km"
  #   ),
  #   start = 0.7,
  #   end = 0.1
  # ) +
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
  width = 400, height = 350, units = "mm",
  limitsize = F
)
