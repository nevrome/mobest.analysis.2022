library(magrittr)

# remove individuals with too few SNPs after SNP filtering
post_snp_selection_individuals <- poseidonR::read_janno(
  "data/genotype_data/snp_subsets/filtered_snp_selection_pre_ind_correction/"
  ) %>%
  dplyr::filter(Nr_SNPs >= 20000) %>%
  dplyr::select(Poseidon_ID)

save(
  post_snp_selection_individuals,
  file = "data/genotype_data/post_snp_selection_individuals.RData"
)

post_snp_selection_individuals %>%
  dplyr::transmute(
    ind = paste0("<", Poseidon_ID, ">")
  ) %>%
  readr::write_delim(
    file = "code/01_data_preparation/ind_list_post_snp_selection.txt",
    delim = " ",
    col_names = FALSE
  )
