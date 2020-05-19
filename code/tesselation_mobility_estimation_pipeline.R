library(magrittr)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final
load("data/spatial/area.RData")
load("data/spatial/mobility_regions.RData")

input_list <- lapply(1:5, function(age_resampling_run) {
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
    # merge overlapping sample points
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

pred_grid <- mobest::create_prediction_grid(area, spatial_cell_size = 50000, time_layers = seq(-7500, -500, 100)) %>%
  dplyr::mutate(
    x = x / 1000,
    y = y / 1000
  )

res <- pbapply::pblapply(1:length(input_list), function(i) {
  
  input <- input_list[[i]]
  
  raw_voro_output <- bleiglas::tessellate(
    input[, c("id", "x", "y", "z")],
    x_min = min(pred_grid$x) - 150, x_max = max(pred_grid$x) + 150, 
    y_min = min(pred_grid$y) - 150, y_max = max(pred_grid$y) + 150, 
    options = ""
  )
  
  polygon_edges <- bleiglas::read_polygon_edges(raw_voro_output) 
  
  attributed_pred_grid <- bleiglas::attribute_grid_points_to_polygons(pred_grid, polygon_edges)
  
  spu <- attributed_pred_grid %>% data.table::merge.data.table(
    input[, c("id", "PC1", "PC2", "PC3", "PC4")], by.x = "polygon_id", by.y = "id"
  )
  
  spu$run <- i
  
  return(spu)
  
}, cl = 5)

res_total <- res %>% data.table::rbindlist()

res_columns <- res_total %>%
  dplyr::group_by(
    point_id
  ) %>%
  dplyr::summarise(
    x = dplyr::first(x),
    y = dplyr::first(y),
    z = dplyr::first(z),
    PC1 = mean(PC1),
    PC2 = mean(PC2),
    PC3 = mean(PC3),
    PC4 = mean(PC4)
  )

# library(ggplot2)
# res_columns %>% ggplot() + 
#   geom_raster(
#     aes(x, y, fill = mean_PC1)
#   ) +
#   facet_wrap(~z)

interpol_grid_tess <- res_columns %>%
  tidyr::pivot_longer(cols = tidyselect::all_of(c("PC1", "PC2", "PC3", "PC4")), names_to = "dependent_var_id", values_to = "mean") %>%
  dplyr::mutate(
    independent_table_id = "test",
    kernel_setting_id = "test",
    pred_grid_id = "test",
    independent_table_type = "test",
    sd = 0,
    x = x*1000,
    y = y*1000
  )

interpol_grid_tess_origin <- mobest::search_spatial_origin(interpol_grid_tess)


load("data/spatial/mobility_regions.RData")
mobility_proxy <- mobest::estimate_mobility(interpol_grid_tess_origin, mobility_regions)

mobility_proxy %>%
  ggplot() +
  geom_line(
    aes(x = z, y = mean_km_per_decade, color = angle_deg),
    size = 2
  ) +
  facet_wrap(~region_id) +
  scale_color_gradientn(
    colours = c("orange", "red", "red", "darkgreen", "darkgreen", "#0072B2", "#0072B2", "orange"), 
    guide = F
  )


