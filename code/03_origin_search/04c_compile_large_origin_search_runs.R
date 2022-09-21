library(magrittr)

#### data ####

load("data/genotype_data/janno_final.RData")

#### load, merge and modify data ####

origin_vectors_raw <- lapply(
  list.files(
    "data/origin_search/large_origin_search", 
    pattern = "^ovs\\_sample\\_[0-9]+",
    full.names = T
  ), function(x) {
    load(x)
    origin_vectors
  }
) %>% dplyr::bind_rows()

origin_vectors_region <- dplyr::left_join(
  origin_vectors_raw,
  janno_final %>% dplyr::select(Poseidon_ID, region_id),
  by = c("search_id" = "Poseidon_ID")
)

origin_vectors <- dplyr::mutate(
    origin_vectors_region,
    dplyr::across(tidyselect::any_of(
      c("field_x", "field_y", "search_x", "search_y", "ov_x", "ov_y", "ov_dist", "ov_dist_se", "ov_dist_sd")
    ), function(x) { x/1000 })
  )

packed_origin_vectors <- mobest::pack_origin_vectors(
  origin_vectors,
  region_id, multivar_method, search_time
)

origin_summary <- mobest::summarize_origin_vectors(
  packed_origin_vectors,
  region_id, multivar_method, search_time,
  window_start = -8000,
  window_stop = 2000,
  window_width = 400,
  window_step = 50,
  dist_fraction_width = 500
)

no_data_windows <- mobest::find_no_data_windows(
  origin_summary,
  region_id, multivar_method, search_time
)

# library(ggplot2)
# ggplot() +
#   geom_line(
#     data = origin_summary %>% dplyr::filter(multivar_method == "pca5"),
#     mapping = aes(z, ov_dist)
#   ) +
#   geom_point(
#     data = packed_origin_vectors %>% dplyr::filter(multivar_method == "pca5"),
#     mapping = aes(search_z, ov_dist, color = ov_angle_deg)
#   ) +
#   facet_wrap(search_time~region_id) +
#   scale_color_gradientn(colours = c("red", "green", "blue", "red"))

#### save output ####

save(packed_origin_vectors, file = "data/origin_search/packed_origin_vectors.RData")
save(origin_summary, file = "data/origin_search/origin_summary.RData")
save(no_data_windows, file = "data/origin_search/no_data_windows.RData")
