library(magrittr)

#### prepare for run ####

data_path <- "/projects1/coest_mobility/coest.interpol.2020/data"
dataset <- "1240K_HumanOrigins"

readr::write_lines(c(
    paste("genotypename:", file.path(data_path, "1240K_HumanOrigins", paste0(dataset, ".geno"))),
    paste("snpname:", file.path(data_path, "1240K_HumanOrigins", paste0(dataset, ".snp"))),
    paste("indivname:", file.path(data_path, "1240K_HumanOrigins", paste0(dataset, ".ind"))),
    paste("evecoutname:", file.path(data_path, "pca", paste0("pca.", dataset, ".evec"))),
    paste("evaloutname:", file.path(data_path, "pca", paste0("pca.", dataset, ".eval"))),
    paste("poplistname:", file.path(data_path, "population_lists", "PCA_6.pops")),
    "lsqproject: YES",
    "numoutevec: 10",
    "numthreads: 32"
  ),
  path = file.path(data_path, "pca", paste0("pca.", dataset, ".params.txt"))
)

#### run smartpca ####

system(paste("smartpca -p", file.path(data_path, "pca", paste0("pca.", dataset, ".params.txt"))))

#### read and modify smartpca output ####

# smartpca output
pca <- readr::read_delim(
  file.path(data_path, "pca", paste0("pca.", dataset, ".evec")),
  " ",
  trim_ws = T
)
eigenvalues <- colnames(pca)[2:(ncol(pca) - 1)]
colnames(pca) <- c("Name", paste0("PC", 1:10), "group")

# reference groups
reference_groups <- readLines(file.path(data_path, "population_lists", "PCA_6.pops"))
pca <- pca %>%
  dplyr::mutate(
    reference_group = group %in% reference_groups
  )

# save pca results
save(pca, file = file.path(data_path, "pca", "pca.1240K_HumanOrigins.RData"))
