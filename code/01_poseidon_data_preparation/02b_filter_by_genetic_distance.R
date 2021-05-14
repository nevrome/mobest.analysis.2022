library(magrittr)

# get and prepare data
dist_values <- readr::read_tsv(
  "data/poseidon_data/identical_filter/plink.mdist",
  col_names = F,
  col_types = readr::cols(.default = readr::col_double())
) %>% as.data.frame()
dist_labels <- readr::read_tsv(
  "data/poseidon_data/identical_filter/plink.mdist.id",
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

load("data/poseidon_data/janno_pre_mds.R")

spatiotemporal_distances <- janno_pre_mds %$%
  data.frame(
    IID1 = rep(Individual_ID, each = length(unique(Individual_ID))),
    IID2 = Individual_ID,
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

genetic_spatiotemporal_distances <- genetic_distances %>% 
  dplyr::left_join(
    spatiotemporal_distances,
    by = c("IID1", "IID2")
  )

distances <- genetic_spatiotemporal_distances %>%
  dplyr::mutate(
    string_distance = stringdist::stringdist(IID1, IID2, method = "lv")
  )

# distances %>%
#   ggplot() +
#   geom_hex(
#     aes(x = spatial_distance, y = genetic_distance, col = ..count..),
#   )

distances %>%
  ggplot() +
  geom_hex(
    aes(x = string_distance, y = genetic_distance, col = ..count..)
  )

# distances %>%
#   dplyr::filter(
#     genetic_distance > 0.3,
#     genetic_distance < 0.33
#   ) %>%
#   ggplot() +
#   geom_hex(
#     aes(x = spatial_distance, y = genetic_distance, col = ..count..),
#   )

small_genetic_distances <- distances %>% 
  dplyr::filter(genetic_distance < 0.2)

genetic_identicals_groups <- small_genetic_distances %>% 
  dplyr::select(IID1, IID2) %>%
  igraph::graph_from_data_frame() %>%
  igraph::components() %$%
  membership %>%
  as.list() %>%
  data.frame() %>%
  t %>%
  tibble::as_tibble(rownames = "id") %>%
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

# this should be done on SNP count!!
group_representatives <- identical_groups %>%
  dplyr::group_by(group) %>%
  dplyr::filter(
    dplyr::row_number() == 1
  ) %>%
  dplyr::ungroup() %$% 
  id

duplicates_to_remove <- setdiff(identical_groups$id, group_representatives)


