library(magrittr)

dist_file <- "data/poseidon_data/mds/poseidon_extracted.pruned.genome"

genetic_distances <- readr::read_fwf(
  dist_file,
  readr::fwf_empty(
    dist_file, 
    col_names = c("FID1", "IID1", "FID2", "IID2", "RT", "EZ", "Z0", "Z1", "Z2", "PI_HAT", "PHE", "DST", "PPC", "RATIO")
  ),
  skip = 1
) %>%
  dplyr::select(
    c("FID1", "IID1", "FID2", "IID2", "genetic_distance" = "DST")
  )

load("data/poseidon_data/janno_pre_mds.R")

spatial_distances <- janno_pre_mds %$%
  data.frame(
    IID1 = rep(Individual_ID, each = length(unique(Individual_ID))),
    IID2 = Individual_ID,
    spatial_distance = 
      data.frame(x, y) %>% 
      stats::dist(method = "euclidean") %>% 
      as.matrix() %>% 
      as.vector() %>%
      `/`(1000)
  )

hu <- genetic_distances %>% 
  dplyr::left_join(
    spatial_distances,
    by = c("IID1", "IID2")
  )

hu %>% 




