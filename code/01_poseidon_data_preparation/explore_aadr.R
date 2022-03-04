library(magrittr)

load("data/spatial/epsg3035.RData")
load("data/spatial/research_area.RData")

aadr_raw <- readr::read_tsv("https://reichdata.hms.harvard.edu/pub/datasets/amh_repo/curated_releases/V50/V50.0/SHARE/public.dir/v50.0_1240K_public.anno", na = c("", ".."))

aadr_renamed <- aadr_raw %>%
  dplyr::select(
    Poseidon_ID = `Version ID`,
    Latitude = Lat.,
    Longitude = Long.,
    Date_BP_Median_Derived = `Date mean in BP in years before 1950 CE [OxCal mu for a direct radiocarbon date, and average of range for a contextual date]`,
    Nr_SNPs = `SNPs hit on autosomal targets`,
    Xcontam = `Xcontam ANGSD MOM point estimate (only if male and ≥200)`,
    Genetic_Sex = Sex,
    ASSESSMENT
  ) %>%
  dplyr::mutate(
    Date_BC_AD_Median_Derived = -Date_BP_Median_Derived + 1950
  )

# lacking spatial info filter
aadr_spatial_positions <- aadr_renamed %>%
  dplyr::filter(
    !is.na(Latitude) & !is.na(Longitude)
  )

# temporal filter
aadr_age_filtered <- aadr_spatial_positions %>% dplyr::filter(
  !is.na(Date_BC_AD_Median_Derived) & Date_BC_AD_Median_Derived > -8000
)

# spatial filter 
aadr_spatial <- aadr_age_filtered %>% 
  sf::st_as_sf(
    coords = c("Longitude", "Latitude"), 
    crs = sf::st_crs(4326),
    remove = FALSE
  ) %>%
  sf::st_transform(epsg3035) %>%
  sf::st_intersection(
    research_area
  ) %>%
  # transform back to tibble
  dplyr::mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]
  ) %>%  
  sf::st_drop_geometry()

# Nr_SNPs: should be >= 20000 SNPs
aadr_QC <- aadr_spatial %>% dplyr::filter(
  Nr_SNPs >= 20000
)
# Xcontam: if male, then should not be higher then 10%
aadr_QC <- aadr_QC %>% dplyr::filter(
  is.na(Xcontam) | Genetic_Sex == "F" | (Genetic_Sex == "M" & Xcontam < 0.1)
)
# Genetic_Sex: Individuals with unknown genetic sex should be removed
aadr_QC <- aadr_QC %>% dplyr::filter(Genetic_Sex != "U")
# Indicated as contaminated: Individuals which are indicated as potentially contaminated
# in their ID should be removed
aadr_QC <- aadr_QC %>% dplyr::filter(
  ASSESSMENT == "PASS"
)

