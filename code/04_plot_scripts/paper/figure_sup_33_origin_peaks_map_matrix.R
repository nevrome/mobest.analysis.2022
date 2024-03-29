library(magrittr)
library(ggplot2)

load("data/origin_search/packed_origin_vectors.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")

packed_origin_vectors <- packed_origin_vectors %>%
  dplyr::filter(multivar_method == "mds2", search_time == -667)

vecs_grouped <- packed_origin_vectors %>%
  dplyr::mutate(
    time_window = as.numeric(as.character(cut(
      search_z, 
        breaks = seq(-7500, 1500, 500), 
        labels = seq(-7250, 1250, 500),
        include.lowest = T
      ))),
    time_window_label = paste0(
      abs(time_window - 250), " - ", abs(time_window + 250), " ", ifelse(time_window < 0, "BC", "AD")
      )
  ) %>%
  dplyr::group_by(time_window) %>%
  dplyr::mutate(
    search_z_in_window = search_z - time_window + 250
  ) %>%
  dplyr::arrange(search_z_in_window) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    dplyr::across(
      tidyselect::all_of(c("field_x", "field_y", "search_x", "search_y")),
      \(x) x*1000
    )
  )

p <- ggplot() +
  facet_wrap(
    ~time_window, nrow = 6, ncol = 3,
    labeller = labeller(
      time_window = as_labeller(
        vecs_grouped %>%
          dplyr::select(time_window, time_window_label) %>%
          unique %>%
          tibble::deframe()
      )
    )
  ) +
  geom_sf(data = extended_area, fill = "black", color = NA) +
  geom_segment(
    data = vecs_grouped,
    aes(x = search_x, y = search_y, xend = field_x, yend = field_y),
    color = "white",
    size = 0.4,
    alpha = 0.6
  ) +
  geom_point(
    data = vecs_grouped,
    aes(x = field_x, y = field_y, color = search_z_in_window),
    size = 0.8) +
  geom_point(
    data = vecs_grouped,
    aes(x = search_x, y = search_y),
    color = "green", size = 0.8, shape = 15) +
  scale_color_gradient(
    limits = c(0, 500),
    low = "red",
    high = "#FFC93C"
  ) +
  guides(
    color = guide_colorbar(title = "Time in time window", barwidth = 25)
  ) +
  #geom_sf(data = extended_area, fill = NA, colour = "black") + 
  theme_bw() +
  coord_sf(
    expand = FALSE,
    crs = epsg3035
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 12),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 15),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#BFD5E3")
  )

ggsave(
  paste0("plots/figure_sup_33_origin_peaks_map_matrix.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 350, height = 580, units = "mm",
  limitsize = F
)
