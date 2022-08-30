library(magrittr)

update_coords <- function(df, x, y, incrs = epsg3035, outcrs = "+proj=robin") {
  df_sf <- df %>%
    sf::st_as_sf(coords = c(x, y), crs = incrs) %>%
    sf::st_transform(outcrs)
  sf::st_coordinates(df_sf) %>%
    as.data.frame() %>%
    set_colnames(c(x, y))
}

load("data/spatial/epsg3035.RData")
load("data/genotype_data/janno_final.RData")
load("data/origin_search/packed_origin_vectors.RData")

samples <- janno_final %>%
  dplyr::left_join(
    packed_origin_vectors %>%
      dplyr::filter(multivar_method == "mds2" & search_time == -667),
    by = c("Poseidon_ID" = "search_id") 
  )

samples$field_x[is.na(samples$field_x)] <- 0
samples$field_y[is.na(samples$field_y)] <- 0

samples$field_x <- samples$field_x * 1000
samples$field_y <- samples$field_y * 1000

readr::write_csv(
  x = cbind(
      samples$Poseidon_ID,
      samples %>% update_coords("x", "y"),
      samples$Date_BC_AD_Median_Derived,
      samples$C1_mds_u,
      samples$C2_mds_u,
      purrr::map_chr(samples$Group_Name, (\(x) x[[1]])),
      samples %>% update_coords("field_x", "field_y")
  ),
  file = "~/Desktop/samples.csv",
  col_names = FALSE
)

sf::read_sf("~/Downloads/ne_50m_coastline/ne_50m_coastline.shp") %>%
  sf::st_transform("+proj=robin") %$%
  purrr::map2_dfr(
    seq_len(length(geometry)), geometry,
    function(a1, b) {
      purrr::map2_dfr(
        seq_len(length(b)), b,
        function(a2, c) {
          c %>%
            as.data.frame() %>% setNames(c("x", "y")) %>% tibble::as_tibble() %>%
            tibble::add_column(id = paste0(a1, "_", a2), .before = "x")
        }
      )
    }
  ) %>%
  readr::write_csv(
    file = "~/Desktop/land.csv",
    col_names = FALSE
  )

