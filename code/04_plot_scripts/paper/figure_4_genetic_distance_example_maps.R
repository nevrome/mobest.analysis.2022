library(magrittr)
library(ggplot2)

load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
load("data/origin_search/janno_search.RData")
load("data/origin_search/closest_points_examples.RData")
load("data/origin_search/distance_grid_examples.RData")

p <- ggplot() +
  facet_wrap(
    ~Individual_ID,
    ncol = 2,
    labeller = ggplot2::labeller(Individual_ID = c(
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
    data = distance_grid_examples %>%
      dplyr::group_by(Individual_ID, x, y, z) %>%
      dplyr::summarise(
        gen_dist = mean(gen_dist),
        .groups = "drop"
      ),
    mapping = aes(x = x, y = y, fill = gen_dist),
  ) +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  geom_sf(data = extended_area, fill = NA, colour = "black") +
  geom_point(
    data = janno_search,
    mapping = aes(x = x, y = y),
    colour = "red",
    size = 5
  ) +
  geom_point(
    data = closest_points_examples,
    mapping = aes(x = x, y = y),
    colour = "orange",
    size = 4,
    shape = 4,
    stroke = 2
  ) +
  geom_text(
    data = data.frame(
      Individual_ID = janno_search$Individual_ID,
      plot_label = LETTERS[seq_len(nrow(janno_search))]
    ),
    aes(label = plot_label),
    x = -Inf, y = Inf, hjust = -0.4, vjust = 1.4,
    inherit.aes = FALSE,
    size = 7
  ) +
  theme_bw() +
  coord_sf(
    expand = FALSE,
    crs = epsg3035
  ) +
  guides(
    fill = guide_colorbar(title = "Genetic distance  ", barwidth = 25)
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

