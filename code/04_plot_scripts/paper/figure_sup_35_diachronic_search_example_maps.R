library(magrittr)
library(ggplot2)

load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
load("data/origin_search/diachronic_janno_search_selected_individuals.RData")
load("data/origin_search/diachronic_search_result_selected_individuals.RData")

loc <- diachronic_location_examples

p <- ggplot() +
  facet_wrap(
    ~search_time,
    ncol = 3,
    labeller = ggplot2::labeller(
      search_time = purrr::map_chr(unique(loc$search_time), function(x) {
      paste0("Stuttgart: 5250BC (", abs(x), " BC)")
    }) %>% set_names(unique(loc$search_time))
  )) +
  geom_sf(data = extended_area, fill = "black") +
  geom_raster(
    data = loc,
    mapping = aes(x = field_x, y = field_y, fill = probability),
  ) +
  scale_fill_viridis_c(
    option = "mako", direction = -1, labels = scales::comma,
    limits = c(0, 0.0015)
  ) +
  geom_sf(data = extended_area, fill = NA, colour = "black") +
  geom_point(
    data = janno_search,
    mapping = aes(x = x, y = y),
    fill = "red", colour = "black", shape = 21,
    size = 5
  ) +
  geom_point(
    data = loc %>% 
      dplyr::group_by(search_id) %>%
      dplyr::slice_max(probability) %>%
      dplyr::ungroup(),
    mapping = aes(x = field_x, y = field_y),
    fill = "orange", colour = "black", shape = 21,
    size = 3
  ) +
  theme_bw() +
  coord_sf(
    expand = FALSE,
    crs = epsg3035
  ) +
  guides(
    fill = guide_colorbar(
      title = "Similarity probability        ", barwidth = 25
      ) #, label = FALSE, ticks = FALSE)
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 15),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(size = 15),
    strip.text = element_text(size = 15),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#BFD5E3")
  )

ggsave(
  "plots/figure_sup_35_diachronic_search_example_maps.pdf",
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 350, height = 410, units = "mm",
  limitsize = F
)

