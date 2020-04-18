library(magrittr)

mds <- readr::read_delim("data/mds/plink.mds", " ", trim_ws = T) %>%
  dplyr::select(-X8)

inquant <- function(x) {
  x >= quantile(x, probs = 0.025) & x <= quantile(x, probs = 0.975)
}

mds %>%
  dplyr::mutate(
    C1_in_95 = inquant(C1),
    C2_in_95 = inquant(C2),
    C3_in_95 = inquant(C3),
    C4_in_95 = inquant(C4),
    in_95 = C1_in_95 & C2_in_95 & C3_in_95 & C4_in_95
  ) %>%
  dplyr::filter(
    !in_95
  ) %$%
  paste(IID, IID) %>% 
  writeLines("code/mds/outlier_individuals.txt")

