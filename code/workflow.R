pw <- readLines("pw.txt")
u <- "clemens_schmid"
h <- "daghead1.eva.mpg.de"
lb <- "~/agora/mobest.analysis.2020"
cb <- "/mnt/archgen/users/schmid/mobest.analysis.2020"
ssp <- "/mnt/archgen/users/schmid/mobest.analysis.2020/singularity_qsub.sh"

bash <- function(x) { eva.cluster::run_local_bash_command(
  paste("./singularity_mobest.sif", x), where_run_path = lb
)}
up <- function(...) { eva.cluster::cluster_up(
    ..., user = u, host = h, pw = pw, local_base = lb, cluster_base = cb, equal_path = T
)}
down <- function(...) { eva.cluster::cluster_down(
    ..., user = u, host = h, pw = pw, local_base = lb, cluster_base = cb, equal_path = T
)}
qsub <- function(x) { eva.cluster::run_cluster_qsub_script(
    x, where_run_path = cb, host = h, user = u, pw = pw
)}
singularity_qsub <- function(x) { eva.cluster::run_cluster_singularity_script(
  x, where_run_path = cb, singularity_script_path = ssp, cores = 16, memory = 50, 
  host = h, user = u, pw = pw
)}

#### singularity container ####

system("./singularity_build_sif.sh")
up("singularity_mobest.sif")

#### download output of the pipeline ####

down("data/")
down("plots/")

#### run all plotting scripts ####

purrr::walk(
  list.files("code/04_plot_scripts/paper", pattern = "\\.R", full.names = T),
  \(x) {
    message(x)
    source(x)
    rm(list = ls())
  }
)
