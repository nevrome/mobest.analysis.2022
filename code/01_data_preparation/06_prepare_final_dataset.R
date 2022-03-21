library(magrittr)

load("data/genotype_data/janno_without_identicals.RData")
load("data/genotype_data/post_snp_selection_individuals.RData")
load("data/spatial/mobility_regions.RData")
load("data/spatial/epsg3035.RData")

# construct up-to-date state of the janno table
# the join should make sure that the individual order is the same as in the in-
# and therefore output of the multivar stats below 
janno <- post_snp_selection_individuals %>%
  dplyr::left_join(
    janno_without_identicals,
    by = "Poseidon_ID"
  ) %>%
  dplyr::select(-source_file)

# add spatial and temporal grouping and coordinates
janno_spatial <- janno %>%
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
  
janno_context_complete <- janno_spatial %>%
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

# read results of the multivariate analyses

# mds
read_mds <- function(x) {
  readr::read_fwf(
    file = x, 
    col_positions = readr::fwf_empty(
      x,
      skip = 1,
      col_names = c("FID", "IID", "SOL", paste0("C", 1:10)),
      n = 3000
    ),
    trim_ws = T,
    col_types = "ccdddddddddd",
    skip = 1
  )
}

mds_unfiltered_snp_selection <- read_mds(
  "data/genotype_data/multivariate_analysis/MDS_unfiltered_snp_selection/mds.mds"
) %>% dplyr::select(-IID, -FID, -SOL) %>%
  dplyr::rename_with(
    .cols = tidyselect::starts_with("C"),
    function(x) { paste0(x, "_mds_u") }
  )

mds_filtered_snp_selection <- read_mds(
  "data/genotype_data/multivariate_analysis/MDS_filtered_snp_selection/mds.mds"
) %>% dplyr::select(-IID, -FID, -SOL) %>%
  dplyr::rename_with(
    .cols = tidyselect::starts_with("C"),
    function(x) { paste0(x, "_mds_f") }
  )

# mds_filtered_snp_selection %>%
#   ggplot() +
#   geom_point(aes(x = C1_mds_f, y = C3_mds_f, color = grepl(".SG", IID)))

# pca
load("data/genotype_data/multivariate_analysis/PCA_unfiltered_snp_selection/pca_out.RData")
pca_unfiltered_snp_selection <- pca_out$pca.sample_coordinates %>%
  tibble::as_tibble() %>%
  dplyr::select(-Group, -Class) %>%
  dplyr::rename_with(
    .cols = tidyselect::starts_with("PC"),
    function(x) { paste0(gsub("P", "", x), "_pca_u") }
  )

load("data/genotype_data/multivariate_analysis/PCA_filtered_snp_selection/pca_out.RData")
pca_filtered_snp_selection <- pca_out$pca.sample_coordinates %>%
  tibble::as_tibble() %>%
  dplyr::select(-Group, -Class) %>%
  dplyr::rename_with(
    .cols = tidyselect::starts_with("PC"),
    function(x) { paste0(gsub("P", "", x), "_pca_f") }
  )

# emu
emu_unfiltered_snp_selection <- readr::read_delim(
  "data/genotype_data/multivariate_analysis/EMU_unfiltered_snp_selection/emu_out.txt.eigenvecs",
  delim = " ", col_names = FALSE, col_types = "dddddddddd"
) %>% dplyr::rename_with(
  .cols = tidyselect::starts_with("X"),
  function(x) { paste0("C", gsub("X", "", x), "_emu_u") }
)

emu_filtered_snp_selection <- readr::read_delim(
  "data/genotype_data/multivariate_analysis/EMU_filtered_snp_selection/emu_out.txt.eigenvecs",
  delim = " ", col_names = FALSE, col_types = "dddddddddd"
) %>% dplyr::rename_with(
  .cols = tidyselect::starts_with("X"),
  function(x) { paste0("C", gsub("X", "", x), "_emu_f") }
)

# merge multivariate analysis output with janno
janno_multivar <- dplyr::bind_cols(
  janno_context_complete,
  mds_unfiltered_snp_selection,
  mds_filtered_snp_selection,
  pca_unfiltered_snp_selection,
  pca_filtered_snp_selection,
  emu_unfiltered_snp_selection,
  emu_filtered_snp_selection
)

# finalize data
janno_final <- janno_multivar %>% dplyr::arrange(
  Date_BC_AD_Median_Derived
)

save(janno_final, file = "data/genotype_data/janno_final.RData")
