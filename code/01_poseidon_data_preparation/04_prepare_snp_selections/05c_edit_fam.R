# read current fam file
fam <- readr::read_delim(
  "data/genotype_data/clean/clean1.fam",
  delim = " ", col_names = FALSE
)

#
fam$X6 <- as.integer(grepl(".SG", fam$X2)) + 1

readr::write_delim(
  fam,
  file = "data/genotype_data/clean/clean1.fam", 
  delim = " ", col_names = FALSE, quote = "none"
)
