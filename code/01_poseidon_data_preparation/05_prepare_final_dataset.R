# mkdir data/poseidon_data/poseidon_extracted; scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/poseidon_data/poseidon_extracted/poseidon2_extracted.janno data/poseidon_data/poseidon_extracted/poseidon2_extracted.janno
# scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/poseidon_data/mds/poseidon2_extracted.pruned.mds data/poseidon_data/mds/poseidon2_extracted.pruned.mds

library(magrittr)

load("data/spatial/mobility_regions.RData")
load("data/spatial/epsg102013.RData")

# read active data
janno <- poseidon2::read_janno("data/poseidon_data/poseidon_extracted/poseidon2_extracted.janno")
mds <- readr::read_delim("data/poseidon_data/mds/poseidon2_extracted.pruned.mds", " ", trim_ws = T) %>%
  dplyr::select(IID, C1, C2)

# run age processing
janno_age <- janno %>% poseidon2::process_age()

# merge mds info into dataset
janno_mds <- janno_age %>% 
  dplyr::left_join(
    mds, by = c("Individual_ID" = "IID")
  )

# add spatial and temporal grouping and coordinates
janno_spatial <- janno_mds %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  sf::st_transform(crs = epsg102013)

janno_spatial_regions <- janno_spatial %>% sf::st_intersection(mobility_regions)

sf::write_sf(janno_spatial_regions, dsn = "data/poseidon_data/janno_spatial_filtered.gpkg", driver = "GPKG")

janno_regions <- janno_spatial_regions %>%
  dplyr::mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]
  ) %>%
  sf::st_drop_geometry()

janno_age_groups <- janno_regions %>%
  dplyr::mutate(
    age_group_id = cut(
      Date_BC_AD_Median_Derived, 
      breaks = c(-10000, seq(-8000, 2000, 1000)), 
      labels = c(">-8000", paste0(seq(-8000, 1000, 1000), " - ", seq(-7000, 2000, 1000)))
    )
  )

# finalize data

janno_final <- janno_age_groups

save(janno_final, file = "data/poseidon_data/janno_final.RData")

# scp data/poseidon_data/janno_final.RData schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/poseidon_data/janno_final.RData
