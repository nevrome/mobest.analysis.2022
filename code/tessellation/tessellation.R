library(magrittr)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered
load("data/spatial/research_area.RData")
load("data/spatial/area.RData")

ver <- anno %>%
  dplyr::transmute(
    id = 1:nrow(.),
    x = round(x, 0),
    y = round(y, 0),
    z = calage_center * 1000,
    PC1 = PC1,
    PC2 = PC2
  ) %>%
  dplyr::group_by(
    x, y, z
  ) %>%
  dplyr::filter(
    dplyr::row_number() == 1
  ) %>% dplyr::ungroup()

#### tessellate and read result ####
bb <- sf::st_bbox(research_area)

poly_raw <- bleiglas::tessellate(
  ver[,c("id", "x", "y", "z")],
  x_min = bb[1], x_max = bb[3], 
  y_min = bb[2], y_max = bb[4]
)
polygon_edges <- bleiglas::read_polygon_edges(poly_raw)

#### remove time overemphasis ####
polygon_edges %<>% dplyr::mutate(
  z.a = z.a / 1000,
  z.b = z.b / 1000
)

ver %<>% dplyr::mutate(
  z = z / 1000
)

#### time cuts ####
cut_sufaces <- bleiglas::cut_polygons(
  polygon_edges,
  cuts = seq(-7500, -500, 500),
  crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs"
)

#### crop bleiglas by land area ####
cut_sufaces_cropped <- cut_sufaces %>% sf::st_intersection(area)

#### join bleiglas polygons and metainformation ####
cut_surfaces_info <- cut_sufaces_cropped %>%
  dplyr::left_join(
    ver,
    by = "id"
  )

#### store results ####
save(ver, polygon_edges, file = "data/tessellation/tessellation_3D_data_burial_type.RData")
save(cut_surfaces_info, file = "data/tessellation/tessellation_cut_surfaces_burial_type.RData")
