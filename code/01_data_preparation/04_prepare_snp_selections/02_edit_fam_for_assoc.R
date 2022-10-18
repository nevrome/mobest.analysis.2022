# read current fam file
fam <- readr::read_delim(
  "data/genotype_data/snp_subsets/purified.fam",
  delim = " ", col_names = FALSE
)

# set phenotype flag for capture vs. shotgun
fam$X6 <- as.integer(grepl(".SG", fam$X2)) + 1

# replace fam file with the modified version
readr::write_delim(
  fam,
  file = "data/genotype_data/snp_subsets/purified.fam", 
  delim = " ", col_names = FALSE, quote = "none"
)
