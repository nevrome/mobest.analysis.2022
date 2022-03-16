library(ggplot2)
library(magrittr)

load("data/genotype_data/janno_final.RData")
load("data/origin_search/moving_origin_grid.RData")

janno_final %<>% dplyr::filter(region_id == "Central Europe")

moving_origin_grid_modified <- moving_origin_grid %>%
  dplyr::filter(region_id == "Central Europe") %>%
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

p_curve <- moving_origin_grid_modified %>%
  ggplot() +
  geom_ribbon(
    aes(
      x = z, 
      ymin = 0,
      ymax = number, 
      fill = fraction_type,
    ),
    na.rm = F
  ) +
  geom_rect(
    data = tibble::tibble(xmin = -Inf, ymin = -Inf, ymax = 0, xmax = Inf),
    mapping = aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax
    ),
    fill = "white"
  ) + 
  geom_point(
    data = janno_final %>% dplyr::filter(region_id != "Other region"),
    aes(x = Date_BC_AD_Median_Derived, y = -0.04),
    shape = "|",
    size = 3
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
    xlim = c(-7000, 1000),
    ylim = c(-0.02, 1)
  )

p_legend <- cowplot::get_legend((p_curve + theme(
  legend.position = "right",
  legend.background = element_rect(fill = alpha("white", 0.6)),
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 8),
  legend.key = element_rect(fill = NA, color = NA)
)))

p <- cowplot::ggdraw(p_curve) +
  cowplot::draw_plot(
    p_legend,
    x = 0.73, y = 0.78, 
    width = 0.05, height = 0.05
  )

ggsave(
  paste0("plots/presentation/central_europe_example_fractions.png"),
  plot = p,
  device = "png",
  scale = 0.35,
  dpi = 500,
  width = 850, height = 400, units = "mm",
  limitsize = F
)

