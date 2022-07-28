library(magrittr)
library(ggplot2)

load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
load("data/origin_search/janno_search.RData")
load("data/origin_search/location_examples.RData")

loc <- location_examples_C1toC2_mds_u

p <- ggplot() +
  facet_wrap(
    ~search_id,
    ncol = 2,
    labeller = ggplot2::labeller(search_id = c(
      "Stuttgart_published.DG" = paste(
        "Stuttgart ~5250BC",
        "Early Neolithic, Linear Pottery culture",
        "Lazaridis et al. 2014",
        sep = "\n"
      ),
      "RISE434.SG" = paste(
        "RISE434 ~2750BC",
        "Late Neolithic, Corded Ware culture",
        "Allentoft et al. 2015",
        sep = "\n"
      ),
      "3DT26.SG" = paste(
        "3DRIF-26 ~200AD",
        "Roman Britain",
        "Martiniano et al. 2016",
        sep = "\n"
      ),
      "SI-40.SG" = paste(
        "SI-40 ~1150AD",
        "Medieval Period, Crusades",
        "Haber et al. 2019",
        sep = "\n"
      )
    ))
  ) +
  geom_sf(data = extended_area, fill = "black") +
  geom_raster(
    data = loc,
    mapping = aes(x = field_x, y = field_y, fill = probability),
  ) +
  scale_fill_viridis_c(option = "mako", direction = -1) +
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
    fill = guide_colorbar(title = "Probability  ", barwidth = 25, label = FALSE, ticks = FALSE)
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
  "plots/figure_4_genetic_distance_example_maps.pdf",
  plot = p,
  device = "pdf",
  scale = 0.75,
  dpi = 300,
  width = 300, height = 300, units = "mm",
  limitsize = F
)

