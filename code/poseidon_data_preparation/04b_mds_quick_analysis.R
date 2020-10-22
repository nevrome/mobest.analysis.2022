library(magrittr)
library(ggplot2)

mds <- readr::read_delim("data/poseidon_data/mds/poseidon2_extracted.pruned.mds", " ", trim_ws = T) %>%
  dplyr::select(-X6)

janno <- poseidon2::read_janno("data/poseidon_data/poseidon_extracted/poseidon2_extracted.janno")

load("data/spatial/mobility_regions.RData")
load("data/spatial/epsg102013.RData")

janno_mds_sf <- janno %>% 
  dplyr::left_join(
    mds, by = c("Individual_ID" = "IID")
  ) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  sf::st_transform(crs = epsg102013)

janno_mds_sf_regions <- janno_mds_sf %>% sf::st_intersection(
  mobility_regions
)

janno_regions <- janno_mds_sf_regions %>% sf::st_drop_geometry()

janno_regions <- janno_regions %>%
  dplyr::mutate(
    age_group_id = cut(
      Date_BC_AD_Median, 
      breaks = seq(-10000, 2000, 2000), 
      labels = c(">-8000", paste0(seq(-8000, -2000, 2000), " - ", seq(-6000, -0, 2000)), ">0")
    )
  )

p <- ggplot() +
  geom_point(
    data = janno_regions,
    aes(x = C1, y = C2, color = region_id, shape = age_group_id),
    alpha = 0.7,
    size = 2
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "right"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  scale_shape_manual(
    values = c(
      ">-8000" = 15,
      "-8000 - -6000" = 15,
      "-6000 - -4000" = 17,
      "-4000 - -2000" = 6,
      "-2000 - 0" = 4,
      ">0" = 1
    )
  ) +
  scale_color_manual(
    values = c(
      "Central Europe" = "#999999", 
      "Iberia" = "#E69F00", 
      "Eastern Europe" = "#56B4E9", 
      "Britain and Ireland" = "#009E73", 
      "Turkey" = "#871200",
      "France" = "#F0E442", 
      "Near East" = "#0072B2", 
      "Caucasus" = "#D55E00", 
      "Italy" = "#CC79A7", 
      "Southeastern Europe" = "#2fff00"
    )
  ) +
  guides(
    color = guide_legend(title = ""),
    shape = guide_legend(title = "median age calBC")
  ) +
  coord_fixed() +
  scale_y_reverse()

