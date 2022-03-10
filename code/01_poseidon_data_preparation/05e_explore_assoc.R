library(magrittr)
library(ggplot2)

# CHR     Chromosome
# SNP     SNP ID
# BP      Physical position (base-pair)
# A1      Minor allele name (based on whole sample)
# F_A     Frequency of this allele in cases
# F_U     Frequency of this allele in controls
# A2      Major allele name
# CHISQ   Basic allelic test chi-square (1df)
# P       Asymptotic p-value for this test
# OR      Estimated odds ratio (for A1, i.e. A2 is reference)

read_assoc <- function(x) {
  readr::read_fwf(
    file = x, 
    col_positions = readr::fwf_empty(
      x,
      skip = 1,
      col_names = c("CHR", "SNP", "BP", "A1", "F_A", "F_U", "A2", "CHISQ", "P", "OR"),
      n = 3000
    ),
    trim_ws = T,
    col_types = "ccicddcddd",
    skip = 1
  )
}

assoc <- read_assoc("data/poseidon_data/clean/clean1.assoc")

bad_snps <- assoc %>%
  dplyr::filter(P < 10^-10) %$%
  SNP
  
bim <- readr::read_tsv("data/poseidon_data/clean/clean1.bim", col_names = FALSE)
filtered_bim <- bim %>% dplyr::filter(
  !(X2 %in% bad_snps)
)

readr::write_tsv(filtered_bim, file = "data/poseidon_data/clean/filter.bim", col_names = FALSE)
