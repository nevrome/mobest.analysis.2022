load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered

anno_unnested <- anno %>% 
  dplyr::select(age_prob_distribution_BC, PC1, PC2, PC3, PC4, x, y) %>%
  tidyr::unnest("age_prob_distribution_BC")

anno_slices <- anno_unnested <- anno_unnested %>%
  dplyr::filter(
    age %in% seq(-7500, -500, by = 500)
  ) %>%
  dplyr::mutate(
    age_slice = factor(age, levels = seq(-7500, -500, by = 500))
  )

anno_slices_geo <- anno_slices %>% sf::st_as_sf(
  coords = c("x", "y"),
  crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs"
)

save(anno_slices_geo, file = "data/anno_slices_geo.RData")