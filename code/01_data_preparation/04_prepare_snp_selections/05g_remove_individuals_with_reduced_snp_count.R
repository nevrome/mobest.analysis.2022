library(magrittr)

bubu <- poseidonR::read_janno("data/genotype_data/clean/bubu/")

bubu %>%
  dplyr::filter(Nr_SNPs >= 20000) %>%
  dplyr::transmute(
    ind = paste0("<", Poseidon_ID, ">")
  ) %>%
  readr::write_delim(
    file = "code/01_data_preparation/post_cleaning_ind_list.txt",
    delim = " ",
    col_names = FALSE
  )
