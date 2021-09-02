library(magrittr)

interpol_comparison <- lapply(
  list.files(
    "data/parameter_exploration/crossvalidation", 
    pattern = "mds3_interpol_comparison_[0-9]", 
    full.names = T
  ), function(x) {
    load(x)
    interpol_comparison
  }
) %>% dplyr::bind_rows()

#save(interpol_comparison, file = "data/parameter_exploration/crossvalidation/interpol_comparison.RData")
