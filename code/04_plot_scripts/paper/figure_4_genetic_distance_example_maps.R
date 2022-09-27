library(magrittr)
library(ggplot2)

load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
load("data/origin_search/janno_search_selected_individuals.RData")
load("data/origin_search/search_result_selected_individuals.RData")

loc <- location_examples_C1toC2_mds_u

p <- ggplot() +
  facet_wrap(
    ~search_id,
    ncol = 3,
    labeller = ggplot2::labeller(search_id = c(
      "Stuttgart_published.DG" = paste(
        "Stuttgart: 5250BC (-1500y)",
        "Early Neolithic, Linear Pottery culture",
        "Lazaridis et al. 2014",
        sep = "\n"
      ),
      "RISE434.SG" = paste(
        "RISE434: 2750BC (-300y)",
        "Late Neolithic, Corded Ware culture",
        "Allentoft et al. 2015",
        sep = "\n"
      ),
      "3DT26.SG" = paste(
        "3DRIF-26: 200AD (0y)",
        "Roman Britain",
        "Martiniano et al. 2016",
        sep = "\n"
      ),
      "SI-40.SG" = paste(
        "SI-40: 1150AD (0y)",
        "Medieval Period, Crusades",
        "Haber et al. 2019",
        sep = "\n"
      ),
      "I8341" = paste(
        "I8341: 400BC (-100y)",
        "Iron Age, Greek colony",
        "Olalde et al. 2019",
        sep = "\n"
      ),
      "I8215" = paste(
        "I8215: 550BC (-100y)",
        "Iron Age, Greek colony",
        "Olalde et al. 2019",
        sep = "\n"
      )
    ))
  ) +
  geom_sf(data = extended_area, fill = "black") +
  geom_raster(
    data = loc,
    mapping = aes(x = field_x, y = field_y, fill = probability),
  ) +
  scale_fill_viridis_c(option = "mako", direction = -1, labels = scales::comma) +
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
      title = "Similarity probability  ", barwidth = 25
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
  "plots/figure_4_genetic_distance_example_maps.pdf",
  plot = p,
  device = "pdf",
  scale = 0.9,
  dpi = 300,
  width = 400, height = 260, units = "mm",
  limitsize = F
)

