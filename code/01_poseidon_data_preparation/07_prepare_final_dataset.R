library(magrittr)

load("data/spatial/mobility_regions.RData")
load("data/spatial/epsg3035.RData")

# function to read plink mds files
read_mds <- function(x) {
  readr::read_fwf(
    file = x, 
    col_positions = readr::fwf_empty(
      x,
      skip = 1,
      col_names = c("FID", "IID", "SOL", "C1", "C2", "C3"),
      n = 3000
    ),
    trim_ws = T,
    col_types = "ccdddd_",
    skip = 1
  )
}

# read active data
janno <- poseidonR::read_janno("data/poseidon_data/poseidon_extracted/poseidon_extracted.janno")
mds2 <- read_mds("data/poseidon_data/mds/mds2.mds") %>% 
  dplyr::transmute(
    Individual_ID = IID,
    C1 = C1,
    C2 = C2
  )
mds3 <- read_mds("data/poseidon_data/mds/mds3.mds") %>% 
  dplyr::transmute(
    Individual_ID = IID,
    mds3_C1 = C1,
    mds3_C2 = C2,
    mds3_C3 = C3
  )

# run age processing
janno_age <- janno %>% poseidonR::process_age()

# merge mds info into dataset
janno_mds <- janno_age %>% 
  dplyr::left_join(mds2) %>%
  dplyr::left_join(mds3)

# add spatial and temporal grouping and coordinates
janno_spatial <- janno_mds %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  sf::st_transform(crs = epsg3035)

region_vector <- janno_spatial %>% 
  sf::st_intersects(mobility_regions, sparse = FALSE) %>%
  as.data.frame() %>%
  magrittr::set_names(as.character(mobility_regions$region_id)) %>%
  dplyr::mutate(id = seq_len(nrow(.))) %>%
  tidyr::pivot_longer(setdiff(everything(), one_of("id")), names_to = "region") %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    region_id = if (any(value)) { region[value] } else { "Other region" }
  ) %$%
  factor(region_id, levels = c(levels(mobility_regions$region_id), "Other region"))
  
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
janno_final <- janno_final %>% dplyr::arrange(
  Date_BC_AD_Median_Derived
)

save(janno_final, file = "data/poseidon_data/janno_final.RData")
