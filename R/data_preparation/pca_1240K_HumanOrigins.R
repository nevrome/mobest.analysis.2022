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

# #### read and modify smartpca output ####
# 
# pca <- readr::read_delim(
#   file.path(data_path, "pca", paste0("pca.", dataset, ".evec")),
#   " ", 
#   trim_ws = T
# )
# 
# eigenvalues <- colnames(pca)[2:(ncol(pca) - 1)]
# colnames(pca) <- c("Name", paste0("PC", 1:10), "Group")
# 
# # save pca results
# save(pca, file = "data/pca/pca_europe_result.RData") 
# 
# #### define and store static group dataset ####
# static_groups <- readLines("data/population_lists/PCA_6.pops")
# 
# pca_static <- pca %>% dplyr::filter(
#   Group %in% static_groups
# )
# 
# save(pca_static, file = "data/pca_europe_result_static.RData") 
