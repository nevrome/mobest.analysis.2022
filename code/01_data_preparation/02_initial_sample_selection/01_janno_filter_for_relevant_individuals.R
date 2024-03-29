library(magrittr)

load("data/spatial/epsg3035.RData")
load("data/spatial/research_area.RData")
source("code/01_data_preparation/02_initial_sample_selection/aadr_age_string_parser.R")

# read aadr .anno file to transform it to a minimal .janno file
aadr_raw <- readr::read_tsv("data/genotype_data/aadrv50_1240K/v50.0_1240k_public.anno", na = c("", ".."))

aadr_minimal_janno <- aadr_raw %>%
  dplyr::transmute(
    Poseidon_ID = `Version ID`,
    Group_Name = `Group ID`,
    Country = Country,
    Latitude = Lat.,
    Longitude = Long.,
    Nr_SNPs = `SNPs hit on autosomal targets`,
    # This is not a defined .janno column any more since Poseidon 2.5.0
    Xcontam = as.numeric(`Xcontam ANGSD MOM point estimate (only if male and ≥200)`),
    Genetic_Sex = Sex,
    ASSESSMENT,
    Publication = Publication,
    aadr_age_string = `Full Date: One of two formats. (Format 1) 95.4% CI calibrated radiocarbon age (Conventional Radiocarbon Age BP, Lab number) e.g. 2624-2350 calBCE (3990±40 BP, Ua-35016). (Format 2) Archaeological context range, e.g. 2500-1700 BCE`
  ) %>% cbind(
    split_age_string(.$aadr_age_string)
  ) %>%
  poseidonR::as.janno()

# add a minimal .janno file to the aadr poseidon package (for later extraction)
# poseidonR::write_janno(aadr_minimal_janno, "data/genotype_data/aadrv50_1240K/aadr_poseidon.janno")

# lacking spatial info filter
janno_raw_spatial_positions <- aadr_minimal_janno %>%
  dplyr::filter(
    !is.na(Latitude) & !is.na(Longitude)
  )

# process age information
janno_age <- janno_raw_spatial_positions %>% poseidonR::process_age()

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
  sf::st_transform(epsg3035)

# export for QGIS: Check if the research area is still adequate
# janno_spatial %>%
#   dplyr::select_if(is.list %>% Negate) %>%
#   sf::write_sf(dsn = "data/genotype_data/janno_spatial_pre_filter.gpkg", driver = "GPKG")

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

# Nr_SNPs: should be >= 25000 SNPs
janno_QC <- janno_spatial_filtered_non_sf %>% dplyr::filter(
  Nr_SNPs >= 25000
)
# Xcontam: if male, then should not be higher then 10%
janno_QC <- janno_QC %>% dplyr::filter(
  is.na(Xcontam) | Genetic_Sex == "F" | (Genetic_Sex == "M" & Xcontam < 0.1)
)
# Genetic_Sex: Individuals with unknown genetic sex should be removed
janno_QC <- janno_QC %>% dplyr::filter(Genetic_Sex != "U")
# Individuals which are indicated as potentially borked
# in the AADR dataset should be removed
janno_QC <- janno_QC %>% dplyr::filter(
  ASSESSMENT == "PASS"
)
# Coverage and damage are not too relevant here as filter criteria

janno_initial_selection <- janno_QC

save(janno_initial_selection, file = "data/genotype_data/janno_initial_selection.RData")

# export for QGIS: check every now and then if the new data justifies different region definition
# janno_filtered_final %>% 
#   sf::st_as_sf(
#     coords = c("Longitude", "Latitude"), 
#     crs = sf::st_crs(4326)
#   ) %>%
#   sf::st_transform(epsg3035) %>%
#   dplyr::select_if(is.list %>% Negate) %>%
#   sf::write_sf(dsn = "data/genotype_data/janno_spatial_post_filter.gpkg", driver = "GPKG")

# library(ggplot2)
# ggplot(janno_filtered_final) +
#   geom_point(aes(x, y))

# store ind list for poseidon extraction
tibble::tibble(
  ind = paste0("<", sort(janno_initial_selection$Poseidon_ID), ">")
) %>% 
  readr::write_delim(
    file = "code/01_data_preparation/ind_list_initial_selection.txt",
    delim = " ",
    col_names = FALSE
  )
