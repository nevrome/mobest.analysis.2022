library(magrittr)
library(ggplot2)

mds <- readr::read_delim("data/poseidon_data/mds/poseidon_extracted.pruned.mds", " ", trim_ws = T) %>%
  dplyr::select(-X6)

janno <- poseidon2::read_janno("data/poseidon_data/poseidon_extracted/poseidon_extracted.janno")

load("data/spatial/mobility_regions.RData")
load("data/spatial/epsg102013.RData")
load("data/plot_reference_data/region_id_colors.RData")
load("data/plot_reference_data/age_group_id_shapes.RData")

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
      breaks = c(-10000, seq(-8000, 2000, 1000)), 
      labels = c(">-8000", paste0(seq(-8000, 1000, 1000), " - ", seq(-7000, 2000, 1000)))
    )
  )

ggplot() +
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
    values = age_group_id_shapes
  ) +
  scale_color_manual(
    values = region_id_colors
  ) +
  guides(
    color = guide_legend(title = ""),
    shape = guide_legend(title = "median age calBC")
  ) +
  coord_fixed() +
  scale_y_reverse()

