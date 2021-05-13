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

distances <- genetic_distances %>% 
  dplyr::left_join(
    spatiotemporal_distances,
    by = c("IID1", "IID2")
  )

# distances %>%
#   ggplot() +
#   geom_hex(
#     aes(x = spatial_distance, y = genetic_distance, col = ..count..),
#   )

# distances %>%
#   dplyr::filter(
#     genetic_distance > 0.3,
#     genetic_distance < 0.33
#   ) %>%
#   ggplot() +
#   geom_hex(
#     aes(x = spatial_distance, y = genetic_distance, col = ..count..),
#   )

distances %>% dplyr::filter(
    genetic_distance < 0.2
  ) %>% dplyr::group_by(
    
  )


mean(distances$genetic_distance) - 3*sd(distances$genetic_distance)

