library(magrittr)

update_coords <- function(df, x, y, incrs = epsg3035, outcrs = "+proj=robin") {
  df_selected <- df %>% dplyr::select(.data[[x]], .data[[y]]) %>% dplyr::mutate(i = seq_len(nrow(.)))
  df_without_na <- df_selected %>%
    dplyr::filter(!is.na(.data[[x]]) & !is.na(.data[[y]]))
  df_transformed <- df_without_na %>%
    sf::st_as_sf(coords = c(x, y), crs = incrs) %>%
    sf::st_transform(outcrs) %>%
    sf::st_coordinates() %>%
    as.data.frame() %>%
    set_colnames(c(x, y)) %>%
    dplyr::mutate(i = df_without_na$i)
  df_selected[,"i"] %>%
    dplyr::left_join(df_transformed, by = "i") %>%
    dplyr::select(-i)
}

load("data/spatial/epsg3035.RData")
load("data/genotype_data/janno_final.RData")
load("data/origin_search/packed_origin_vectors.RData")

samples <- janno_final %>%
  dplyr::left_join(
    packed_origin_vectors %>%
      dplyr::mutate(
        search_time_mode = dplyr::case_when(
          search_time == -1015 ~ "high",
          search_time == -667 ~ "default",
          search_time == -378 ~ "low"
        )
      ) %>%
      dplyr::filter(ifelse(multivar_method == "pca5", search_time_mode == "default", TRUE)) %>%
      #dplyr::filter(multivar_method == "mds2" & search_time == -667)
      tidyr::pivot_wider(
        id_cols = search_id,
        names_from = tidyselect::all_of(c("multivar_method", "search_time_mode")),
        values_from = tidyselect::all_of(c("field_x", "field_y"))
      ),
    by = c("Poseidon_ID" = "search_id") 
  )

samples$field_x <- samples$field_x * 1000
samples$field_y <- samples$field_y * 1000

composite_dataset <- tibble::tibble(
  sampleID = samples$Poseidon_ID,
  sampleGroup = purrr::map_chr(samples$Group_Name, (\(x) x[[1]])),
  samples %>% update_coords("x", "y") %>% set_names(c("sampleX", "sampleY")),
  sampleZ = samples$Date_BC_AD_Median_Derived,
  sampleC1MDS = samples$C1_mds_u,
  sampleC2MDS = samples$C2_mds_u,
  sampleC1PCA = samples$C1_pca_proj_u,
  sampleC2PCA = samples$C2_pca_proj_u,
  samples %>% update_coords("field_x", "field_y") %>% set_names(c("sampleFieldX", "sampleFieldY"))
)

readr::write_csv(
  x = composite_dataset,
  file = "~/agora/mobest-hs/assets/data/samples.csv",
  col_names = TRUE
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

