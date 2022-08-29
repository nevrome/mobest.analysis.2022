library(magrittr)

load("data/spatial/epsg3035.RData")

load("data/genotype_data/janno_final.RData")
readr::write_csv(
  x = cbind(
      janno_final$Poseidon_ID,
      janno_final %>% update_coords("x", "y"),
      janno_final$Date_BC_AD_Median_Derived,
      janno_final$C1_mds_u,
      janno_final$C2_mds_u
  ),
  file = "~/Desktop/janno.csv",
  col_names = FALSE
)

update_coords <- function(df, x, y, incrs = epsg3035, outcrs = "+proj=robin") {
  df_sf <- df %>%
    sf::st_as_sf(coords = c(x, y), crs = incrs) %>%
    sf::st_transform(outcrs)
  sf::st_coordinates(df_sf) %>%
    as.data.frame() %>%
    set_colnames(c(x, y))
}

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

