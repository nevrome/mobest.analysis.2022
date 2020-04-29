library(magrittr)

#### load data ####
load("data/anno_1240K_and_anno_1240K_HumanOrigins_mds.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_mds
load("data/spatial/mobility_regions.RData")

#### spatial regions ####
anno_xy <- anno %>% dplyr::select(sample_id, x, y)

anno_xy_spatial <- sf::st_as_sf(
  anno_xy,
  coords = c("x", "y"),
  crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
  remove = FALSE
)

anno_regions <- anno_xy_spatial %>%
  sf::st_intersection(
    mobility_regions
  ) %>%
  tibble::as_tibble() %>%
  dplyr::select(-x, -y, -geometry)

anno <- anno %>%
  dplyr::left_join(
    anno_regions, 
    by = "sample_id"
  )

#### temporal units ####
anno <- anno %>%
  dplyr::mutate(
    age_group_id = cut(
      calage_center, 
      breaks = seq(-10000, 2000, 2000), 
      labels = c(">-8000", paste0(seq(-8000, -2000, 2000), " - ", seq(-6000, -0, 2000)), ">0")
    )
  )

#### save ####
anno_1240K_and_anno_1240K_HumanOrigins_class <- anno
save(anno_1240K_and_anno_1240K_HumanOrigins_class, file = "data/anno_1240K_and_anno_1240K_HumanOrigins_class.RData")

