library(magrittr)

load("data/spatial/epsg102013.RData")
load("data/spatial/area.RData")

janno_raw <- poseidon2::read_janno("data/poseidon_data/poseidon_merged_dataset/poseidon2_merged.janno")

janno_age <- janno_raw %>% poseidon2::process_age()

# lacking info filter
janno_with_sufficient_info <- janno_age %>%
  dplyr::filter(
    !is.na(Latitude) & !is.na(Longitude),
    !is.na(Date_BC_AD_Median_Derived)
  )

# temoral filter
janno_age_filtered <- janno_with_sufficient_info %>% dplyr::filter(
  Date_BC_AD_Median_Derived > -8000
)

# spatial filter -> check every now and then if the new data justifies a bigger/different research area
janno_spatial <- janno_age_filtered %>% 
  sf::st_as_sf(
    coords = c("Longitude", "Latitude"), 
    crs = sf::st_crs(4326)
  ) %>%
  sf::st_transform(epsg102013)

sf::write_sf(janno_spatial, dsn = "data/janno_spatial.gpkg", driver = "GPKG")

janno_spatial_filtered <- janno_spatial %>%
  sf::st_intersection(
    area
  )

janno_spatial_filtered_non_sf <- janno_spatial_filtered %>% 
  # transform back to tibble
  dplyr::mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]
  ) %>%  
  sf::st_drop_geometry()

# QC filter
# Nr_autosomal_SNPs? Coverage_1240K? Endogenous?, Damage?, Xcontam?, mtContam?
# janno_spatial_filtered_non_sf$Nr_autosomal_SNPs %>% hist(breaks = 100)
# janno_spatial_filtered_non_sf$Coverage_1240K %>% hist(breaks = 100)
# janno_spatial_filtered_non_sf$Damage %>% hist(breaks = 100)
# janno_spatial_filtered_non_sf$Xcontam %>% hist(breaks = 100)

# prepare extract list for poseidon2 extract
janno_filtered_final <- janno_spatial_filtered_non_sf

tibble::tibble(
  pop = sapply(janno_filtered_final$Group_Name, function(x) { x[[1]] }),
  ind = janno_filtered_final$Individual_ID
) %>% 
  readr::write_delim(
    path = "code/poseidon_data_preparation/ind_list.txt",
    delim = " ",
    col_names = FALSE
  )
