library(magrittr)

# get and prepare data
dist_values <- readr::read_tsv(
  "data/genotype_data/identical_filter/plink.mdist",
  col_names = F,
  col_types = readr::cols(.default = readr::col_double())
) %>% as.data.frame()
dist_labels <- readr::read_tsv(
  "data/genotype_data/identical_filter/plink.mdist.id",
  col_names = F,
  col_types = readr::cols(.default = readr::col_character())
)

colnames(dist_values) <- dist_labels$X2
rownames(dist_values) <- dist_labels$X2
dist_matrix <- dist_values %>% as.matrix()
dist_matrix[lower.tri(dist_matrix)] <- NA

genetic_distances <- dist_matrix %>%
  reshape2::melt() %>%
  setNames(c("IID1", "IID2", "genetic_distance")) %>%
  dplyr::filter(!is.na(genetic_distance))

load("data/genotype_data/janno_pre_identicals_filter.RData")

spatiotemporal_distances <- janno_pre_identicals_filter %$%
  data.frame(
    IID1 = rep(Poseidon_ID, each = length(unique(Poseidon_ID))),
    IID2 = Poseidon_ID,
    spatial_distance = 
      data.frame(x, y) %>% 
      stats::dist(method = "euclidean") %>% 
      as.matrix() %>% 
      as.vector() %>%
      `/`(1000),
    temporal_distance = 
      data.frame(Date_BC_AD_Median_Derived) %>% 
      stats::dist(method = "euclidean") %>% 
      as.matrix() %>% 
      as.vector()
  )

# merge to distance table
distances <- genetic_distances %>% 
  dplyr::left_join(
    spatiotemporal_distances,
    by = c("IID1", "IID2")
  )

# filter by genetic distance to identify identicals
small_genetic_distances <- distances %>% 
  dplyr::filter(genetic_distance < 0.245)

genetic_identicals_groups <- small_genetic_distances %>% 
  dplyr::select(IID1, IID2) %>%
  igraph::graph_from_data_frame() %>%
  igraph::components() %$%
  membership %>%
  as.list() %>%
  data.frame() %>%
  t %>%
  tibble::as_tibble(rownames = "id", .name_repair = "universal") %>%
  setNames(c("id", "group"))

identical_groups <- genetic_identicals_groups %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(
    n = dplyr::n()
  ) %>%
  dplyr::filter(
    n > 1
  ) %>%
  dplyr::ungroup()

group_representatives <- identical_groups %>%
  dplyr::left_join(
    janno_pre_identicals_filter, by = c("id" = "Poseidon_ID")
  ) %>%
  dplyr::group_by(group) %>%
  dplyr::filter(
    Nr_SNPs == max(Nr_SNPs)
  ) %>%
  dplyr::filter(
    dplyr::row_number() == 1
  ) %>%
  dplyr::ungroup() %$% 
  id

duplicates_to_remove <- setdiff(identical_groups$id, group_representatives)

# remove identicals
`%nin%` <- Negate(`%in%`)
janno_without_identicals <- janno_pre_identicals_filter %>%
  dplyr::filter(Poseidon_ID %nin% duplicates_to_remove)

# save janno_without_identicals for derived applications
save(
  janno_without_identicals,
  file = "data/genotype_data/janno_without_identicals.RData"
)

# write ind_list for extraction
tibble::tibble(
  ind = paste0("<", sort(janno_without_identicals$Poseidon_ID), ">")
) %>% 
  readr::write_delim(
    file = "code/01_genotype_data_preparation/ind_list.txt",
    delim = " ",
    col_names = FALSE
  )
