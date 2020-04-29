library(magrittr)

mds <- readr::read_delim("data/mds/1240K_HumanOrigins.pruned.mds", " ", trim_ws = T) %>%
  dplyr::select(-X8)

inquant <- function(x) {
  x >= quantile(x, probs = 0.025) & x <= quantile(x, probs = 0.975)
}

mds <- mds %>%
  dplyr::mutate(
    C1_in_95 = inquant(C1),
    C2_in_95 = inquant(C2),
    C3_in_95 = inquant(C3),
    C4_in_95 = inquant(C4),
    in_95 = C1_in_95 & C2_in_95 & C3_in_95 & C4_in_95
  ) %>% 
  dplyr::select(IID, in_95)

load("data/anno_1240K.RData")
anno_1240K

mds2 <- mds %>% dplyr::left_join(
  anno_1240K, by = c("IID" = "instance_id")
)

# SG vs. Capture (data_type)

# coverage

# snps_hit_on_autosomes



