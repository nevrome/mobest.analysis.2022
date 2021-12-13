pw <- readLines("pw.txt")
u <- "clemens_schmid"
h <- "daghead1.eva.mpg.de"
lb <- "~/agora/mobest.analysis.2020"
cb <- "/mnt/archgen/users/schmid/mobest.analysis.2020"

up <- function(...) { eva.cluster::cluster_up(
    ..., user = u, host = h, pw = pw, local_base = lb, cluster_base = cb, equal_path = T
)}
down <- function(...) { eva.cluster::cluster_down(
    ..., user = u, host = h, pw = pw, local_base = lb, cluster_base = cb, equal_path = T
)}

#### build and upload singularity container ####

system("./singularity_build_sif.sh")
up("singularity_mobest.sif")

#### download output of the shake pipeline ####

down("ShakeReport.html")
down("plots/")
down("tables/")
down("data/")

#### run all plotting scripts ####

purrr::walk(
  list.files("code/04_plot_scripts/paper", pattern = "\\.R", full.names = T),
  \(x) {
    message(x)
    source(x)
    rm(list = ls())
  }
)
