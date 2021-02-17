library(magrittr)

load("data/spatial/epsg102013.RData")
load("data/spatial/research_area.RData")

janno_raw <- poseidonR::read_janno("/projects1/poseidon/repo/ancient")

# lacking spatial info filter
janno_raw_spatial_positions <- janno_raw %>%
  dplyr::filter(
    !is.na(Latitude) & !is.na(Longitude)
  )

# process age information
janno_age <- janno_raw_spatial_positions %>% poseidon2::process_age()

# temporal filter
janno_age_filtered <- janno_age %>% dplyr::filter(
  !is.na(Date_BC_AD_Median_Derived) & Date_BC_AD_Median_Derived > -8000
)

# spatial filter 
janno_spatial <- janno_age_filtered %>% 
  sf::st_as_sf(
    coords = c("Longitude", "Latitude"), 
    crs = sf::st_crs(4326),
    remove = FALSE
  ) %>%
  sf::st_transform(epsg102013)

# export for QGIS: Check if the research area is still adequate
janno_spatial %>% 
  sf::write_sf(dsn = "data/poseidon_data/janno_spatial_pre_filter.gpkg", driver = "GPKG")

janno_spatial_filtered <- janno_spatial %>%
  sf::st_intersection(
    research_area
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

# Nr_autosomal_SNPs: should be >= 20000 SNPs
janno_QC <- janno_spatial_filtered_non_sf %>% dplyr::filter(
  Nr_autosomal_SNPs >= 20000
)
# Xcontam: if male, then should not be higher then 10%
janno_QC <- janno_QC %>% dplyr::filter(
  is.na(Xcontam) | Genetic_Sex == "F" | (Genetic_Sex == "M" & Xcontam < 0.1)
)
# Genetic_Sex: Individuals with unknown genetic sex should be removed
janno_QC <- janno_QC %>% dplyr::filter(Genetic_Sex != "U")
# Indicated as contaminated: Individuals which are indicated as potentially contaminated
# in their ID should be removed
janno_QC <- janno_QC %>% dplyr::filter(
  !grepl("cont|excluded", x = Individual_ID, ignore.case = T)
)

# Multiple samples from the same individual as well as biologically related individuals are not removed: This should not be an issue for this analysis
# Coverage and damage are not too relevant here as filter criteria

janno_filtered_final <- janno_QC

# export for QGIS: check every now and then if the new data justifies different region definition
janno_filtered_final %>% 
  sf::st_as_sf(
    coords = c("Longitude", "Latitude"), 
    crs = sf::st_crs(4326)
  ) %>%
  sf::st_transform(epsg102013) %>%
  sf::write_sf(dsn = "data/poseidon_data/janno_spatial_post_filter.gpkg", driver = "GPKG")

# store ind list for poseidon extraction
tibble::tibble(
  #pop = sapply(janno_filtered_final$Group_Name, function(x) { x[[1]] }),
  ind = paste0("<", sort(janno_filtered_final$Individual_ID), ">")
) %>% 
  readr::write_delim(
    file = "code/01_poseidon_data_preparation/ind_list.txt",
    delim = " ",
    col_names = FALSE
  )
