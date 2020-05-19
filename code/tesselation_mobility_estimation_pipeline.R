library(magrittr)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final
load("data/spatial/area.RData")
load("data/spatial/mobility_regions.RData")

input_list <- lapply(1:10, function(age_resampling_run) {
  tibble::tibble(
    id = 1:nrow(anno),
    x = anno$x / 1000, 
    y = anno$y / 1000, 
    z = sapply(anno$calage_sample, function(x){ x[age_resampling_run] }),
    PC1 = anno$PC1,
    PC2 = anno$PC2,
    PC3 = anno$PC3,
    PC4 = anno$PC4
  ) %>% 
    dplyr::group_by(x, y, z) %>%
    dplyr::summarise(
      id = dplyr::first(id),
      PC1 = mean(PC1),
      PC2 = mean(PC2),
      PC3 = mean(PC3),
      PC4 = mean(PC4)
    ) %>%
    dplyr::ungroup()
})

dependent <- anno %>% dplyr::select(sample_id, PC1, PC2, PC3, PC4)

pred_grid <- mobest::create_prediction_grid(area, spatial_cell_size = 200000, time_layers = seq(-7500, -500, 500)) %>%
  dplyr::mutate(
    x = x / 1000,
    y = y / 1000
  )

res <- lapply(1:3, function(i) {
  
  input <- input_list[[i]]
  
  raw_voro_output <- bleiglas::tessellate(
    input[, c("id", "x", "y", "z")],
    x_min = min(pred_grid$x) - 150, x_max = max(pred_grid$x) + 150, 
    y_min = min(pred_grid$y) - 150, y_max = max(pred_grid$y) + 150, 
    options = ""
  )
  
  polygon_edges <- bleiglas::read_polygon_edges(raw_voro_output) 
  
  point_polygon <- data.table::data.table(
    point_id = pred_grid$point_id,
    id = bleiglas::attribute_grid_points(polygon_edges, pred_grid)
  )
  
  spu <- point_polygon %>% data.table::merge.data.table(
    pred_grid, by = "point_id"
  ) %>% data.table::merge.data.table(
    input, by = "id"
  )
  
  spu$run <- i
  
  return(spu)
  
})







