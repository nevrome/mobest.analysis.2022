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

genetic_distances <- dist_values %>%
  as.matrix() %>%
  reshape2::melt() %>%
  setNames(c("IID1", "IID2", "genetic_distance"))

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

distances %>% ggplot() +
  geom_hex(
    aes(x = spatial_distance, y = genetic_distance)
  )

schu <- distances %>% dplyr::filter(
  # spatial_distance < 100,
  # temporal_distance < 100
  spatial_distance == 0,
  temporal_distance  == 0
)



