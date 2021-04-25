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
    ncol = 3,
    labeller = ggplot2::labeller(Individual_ID = c(
      "I3025" = paste(
        "? <I3025>",
        "~7300BC, Mesolithic",
        "Brace et al. 2019",
        sep = "\n"
      ),
      "Stuttgart_published.DG" = paste(
        "Stuttgart <Stuttgart_published.DG>",
        "~5250BC, Early Neolithic, Linear Pottery culture",
        "Lazaridis et al. 2014",
        sep = "\n"
      ),
      "RISE434.SG" = paste(
        "? <RISE434.SG>",
        "~2750BC, Late Neolithic, Corded Ware culture",
        "Allentoft et al. 2015",
        sep = "\n"
      ),
      "3DT26.SG" = paste(
        "? <3DT26.SG>",
        "~200AD, Roman Britain",
        "Martiniano et al. 2016",
        sep = "\n"
      ),
      "VK326.SG" = paste(
        "? <VK326.SG>",
        "~930AD, Viking Age",
        "Margaryan et al. 2020",
        sep = "\n"
      ),
      "SI-40.SG" = paste(
        "? <SI-40.SG>",
        "~1150AD, Medieval Period, Crusades",
        "Haber 2019",
        sep = "\n"
      )
    ))
  ) +
  geom_sf(data = extended_area, fill = "black") +
  geom_raster(
    data = distance_grid_examples,
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
    shape = "✖",
    size = 7,
    alpha = 0.7
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
  "plots/figure_4_genetic_distance_example_maps.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.8,
  dpi = 300,
  width = 470, height = 300, units = "mm",
  limitsize = F
)

