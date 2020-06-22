# scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/crossvalidation/interpol_comparison_* .

library(magrittr)

interpol_comparison <- lapply(
  list.files("data/crossvalidation_2", pattern = "interpol_comparison_[0-9]", full.names = T), function(x) {
    load(x)
    interpol_comparison
  }
) %>% dplyr::bind_rows()

save(interpol_comparison, file = "data/parameter_exploration/crossvalidation/interpol_comparison.RData")
