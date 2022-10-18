pw <- readLines("pw.txt")
u <- "clemens_schmid"
h <- "daghead1.eva.mpg.de"
lb <- "~/agora/mobest.analysis.2022"
cb <- "/mnt/archgen/users/schmid/mobest.analysis.2022"

# don't forget to connect to VPN!
up <- function(...) { eva.cluster::cluster_up(
    ..., user = u, host = h, pw = pw, local_base = lb, cluster_base = cb, equal_path = T
)}
down <- function(...) { eva.cluster::cluster_down(
    ..., user = u, host = h, pw = pw, local_base = lb, cluster_base = cb, equal_path = T
)}

#### build and upload singularity container ####

# bash: ./singularity_build_sif.sh
up("singularity_mobest.sif")

#### download output of the shake pipeline ####

down("ShakeReport.html")
down("plots/")
down("tables/")
down("data/genotype_data/poseidon_extracted/")
down("data/")

down("data/parameter_exploration/distance_products.RData")
down("data/parameter_exploration/crossvalidation_best_kernels.RData")
down("data/parameter_exploration/crossvalidation_kernel_comparison.RData")
down("data/parameter_exploration/crossvalidation_multivar_comparison.RData")

#### run all plotting scripts ####

purrr::walk(
  list.files("code/04_plot_scripts/paper", pattern = "\\.R", full.names = T),
  \(x) {
    message(x)
    source(x)
    rm(list = ls())
  }
)
