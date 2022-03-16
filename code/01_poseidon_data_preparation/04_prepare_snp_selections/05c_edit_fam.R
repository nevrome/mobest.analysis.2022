# read current fam file
fam <- readr::read_delim(
  "data/poseidon_data/clean/clean1.fam",
  delim = " ", col_names = FALSE
)

#
fam$X6 <- as.integer(grepl(".SG", fam$X2)) + 1

readr::write_delim(
  fam,
  file = "data/poseidon_data/clean/clean1.fam", 
  delim = " ", col_names = FALSE, quote = "none"
)
