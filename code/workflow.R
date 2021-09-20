pw <- readLines("pw.txt")
u <- "clemens_schmid"
h <- "daghead1.eva.mpg.de"
lb <- "/home/schmid/agora/mobest.analysis.2020"
cb <- "/mnt/archgen/users/schmid/mobest.analysis.2020"
ssp <- "/mnt/archgen/users/schmid/singularity/sge_nevrome_mobest.sh"

bash <- function(x) { eva.cluster::run_local_bash_command(
  x, where_run_path = lb
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
singularity <- function(x) { eva.cluster::run_cluster_singularity_script(
  x, where_run_path = cb, singularity_script_path = ssp, cores = 16, memory = 50, 
  host = h, user = u, pw = pw
)}

#### 01_poseidon_data_preparation ####

bash("./code/01_poseidon_data_preparation/00_fetch_poseidon.sh")
source("./code/01_poseidon_data_preparation/00_prepare_plot_reference_data.R")
source("./code/01_poseidon_data_preparation/00_prepare_spatial_data.R")
up("data/spatial/")

source("./code/01_poseidon_data_preparation/01_janno_filter_for_relevant_individuals.R")
bash("./code/01_poseidon_data_preparation/02_pre_identicals_filter_poseidon_extract.sh")
up("data/poseidon_data/poseidon_extracted_pre_identicals_filter/")
qsub("code/01_poseidon_data_preparation/03_distance_plink.sh")
# wait until cluster run ready
down("data/poseidon_data/identical_filter/plink.mdist",
  "data/poseidon_data/identical_filter/plink.mdist.id")
source("code/01_poseidon_data_preparation/04_filter_by_genetic_distance.R")

bash("./code/01_poseidon_data_preparation/05_poseidon_extract.sh")
up("data/poseidon_data/poseidon_extracted")
qsub("code/01_poseidon_data_preparation/06_mds_plink.sh")
# wait until cluster run ready
down("data/poseidon_data/mds")

source("./code/01_poseidon_data_preparation/07_prepare_final_dataset.R")

up("data/poseidon_data/janno_final.RData")

#### 02_parameter_estimation ####

source("code/02_parameter_estimation/variogram_experiments/variogram_calculation.R")

singularity("code/02_parameter_estimation/laGP_maximum_likelihood_estimation/anisotropic_mle.R")
singularity("code/02_parameter_estimation/laGP_maximum_likelihood_estimation/isotropic_mle.R")
qsub("code/02_parameter_estimation/crossvalidation/sge_parameter_exploration.sh")
# wait until cluster run ready
down("data/parameter_exploration/mle/mlesep_out.RData")
down("data/parameter_exploration/mle/mle_out.RData")
down("data/parameter_exploration/crossvalidation")

source("code/02_parameter_estimation/crossvalidation/modify_crossvalidation_results.R")

#### 03_origin_search #####

source("code/03_origin_search/00_interpolation_and_origin_search_settings.R")
up("data/origin_search/default_kernel.RData")
up("data/origin_search/retrospection_distance.RData")

source("code/03_origin_search/01_interpolation_for_selected_timeslices.R")
source("code/03_origin_search/02_interpolation_at_specific_places_median_age+one_kernel_setting.R")
source("code/03_origin_search/03_interpolation_and_search_for_selected_individuals.R")

#source("code/03_origin_search/04_origin_search_pipeline-median_age+one_kernel_setting.R")

qsub("code/03_origin_search/sge_origin_search.sh")
down("data/origin_search/age_resampling+one_kernel_setting")
source("code/03_origin_search/05b_merge_runs.R")

source("code/03_origin_search/06_moving_window.R")

### 06

up("data/origin_search/default_kernel_mds3.RData")
up("data/origin_search/retrospection_distance_mds3.RData")

down("data/origin_search/age_resampling+one_kernel_setting")

up("data/origin_search/retrospection_distance_retrovar.RData")

