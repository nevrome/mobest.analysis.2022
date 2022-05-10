library(magrittr)

#### data ####

load("data/genotype_data/janno_final.RData")

#### load, merge and modify data ####

origin_vectors_raw <- lapply(
  list.files(
    "data/origin_search/age_resampling+one_kernel_setting", 
    pattern = "^sample_[0-9]+",
    full.names = T
  ), function(x) {
    load(x)
    origin_vectors
  }
) %>% dplyr::bind_rows()

origin_vectors <- dplyr::left_join(
  origin_vectors_raw,
  janno_final %>% dplyr::select(Poseidon_ID, region_id),
  by = c("search_id" = "Poseidon_ID")
)

packed_origin_vectors <- mobest::pack_origin_vectors(origin_vectors, region_id)

origin_summary <- mobest::summarize_origin_vectors(
  packed_origin_vectors,
  region_id,
  window_start = -8000,
  window_stop = 2000,
  window_width = 400,
  window_step = 50
)

no_data_windows <- mobest::find_no_data_windows(origin_summary, region_id)

# library(ggplot2)
# origin_summary %>%
#   ggplot() +
#   geom_point(
#     aes(z, undirected_mean_spatial_distance, color = mean_angle_deg)
#   ) +
#   facet_wrap(~region_id) +
#   scale_color_gradientn(colours = c("red", "green", "blue", "red"))

#### save output ####

save(origin_vectors, file = "data/origin_search/origin_vectors.RData")
save(origin_summary, file = "data/origin_search/origin_summary.RData")
save(no_data_windows, file = "data/origin_search/no_data_windows.RData")
