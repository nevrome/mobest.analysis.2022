library(magrittr)

load("data/spatial/mobility_regions.RData")
load("data/spatial/epsg3035.RData")

# read active data
janno <- poseidonR::read_janno("data/poseidon_data/poseidon_extracted/poseidon_extracted.janno")
mds <- readr::read_delim("data/poseidon_data/mds/poseidon_extracted.pruned.mds", " ", trim_ws = T) %>%
  dplyr::select(IID, C1, C2)

# run age processing
janno_age <- janno %>% poseidonR::process_age()

# merge mds info into dataset
janno_mds <- janno_age %>% 
  dplyr::left_join(
    mds, by = c("Individual_ID" = "IID")
  )

# add spatial and temporal grouping and coordinates
janno_spatial <- janno_mds %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  sf::st_transform(crs = epsg3035)

region_vector <- janno_spatial %>% 
  sf::st_intersects(mobility_regions, sparse = FALSE) %>%
  tibble::as_tibble() %>%
  magrittr::set_names(as.character(mobility_regions$region_id)) %>%
  dplyr::mutate(id = seq_len(nrow(.))) %>%
  tidyr::pivot_longer(setdiff(everything(), one_of("id")), names_to = "region") %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    region_id = if (any(value)) { region[value] } else { NA_character_ }
  ) %$%
  as.character(region_id)

janno_final <- janno_spatial %>%
  dplyr::mutate(
    region_id = region_vector,
    age_group_id = cut(
      Date_BC_AD_Median_Derived, 
      breaks = c(-10000, seq(-8000, 2000, 1000)), 
      labels = c(">-8000", paste0(seq(-8000, 1000, 1000), " - ", seq(-7000, 2000, 1000)))
    ),
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]
  ) %>%
  sf::st_drop_geometry()

# finalize data

save(janno_final, file = "data/poseidon_data/janno_final.RData")
