source("code/technical_helpers.R")

#### 01_poseidon_data_preparation ####

local_script("./code/01_poseidon_data_preparation/00_fetch_poseidon.sh")
source("./code/01_poseidon_data_preparation/00_prepare_plot_reference_data.R")
source("./code/01_poseidon_data_preparation/00_prepare_spatial_data.R")
cluster_up("data/spatial")

source("./code/01_poseidon_data_preparation/01_janno_filter_for_relevant_individuals.R")
local_script("./code/01_poseidon_data_preparation/02_pre_identicals_filter_poseidon_extract.sh")
cluster_up("data/poseidon_data/poseidon_extracted_pre_identicals_filter")
cluster_qsub_script("code/01_poseidon_data_preparation/03_distance_plink.sh")
# wait until cluster run ready
cluster_down(
  "data/poseidon_data/identical_filter/plink.mdist",
  "data/poseidon_data/identical_filter/plink.mdist.id"
)
source("code/01_poseidon_data_preparation/04_filter_by_genetic_distance.R")

local_script("./code/01_poseidon_data_preparation/05_poseidon_extract.sh")
cluster_up("data/poseidon_data/poseidon_extracted")
cluster_qsub_script("code/01_poseidon_data_preparation/06_mds_plink.sh")
# wait until cluster run ready
cluster_down("data/poseidon_data/mds")

source("./code/01_poseidon_data_preparation/07_prepare_final_dataset.R")

cluster_up("data/poseidon_data/janno_final.RData")

#### 02_parameter_estimation ####

source("code/02_parameter_estimation/variogram_experiments/variogram_calculation.R")

cluster_singularity_script("code/02_parameter_estimation/laGP_maximum_likelihood_estimation/anisotropic_mle.R")
cluster_singularity_script("code/02_parameter_estimation/laGP_maximum_likelihood_estimation/isotropic_mle.R")
cluster_qsub_script("code/02_parameter_estimation/crossvalidation/sge_parameter_exploration.sh")
# wait until cluster run ready
cluster_down("data/parameter_exploration/mle/mlesep_out.RData")
cluster_down("data/parameter_exploration/mle/mle_out.RData")
cluster_down("data/parameter_exploration/crossvalidation")

source("code/02_parameter_estimation/crossvalidation/modify_crossvalidation_results.R")

#### 03_origin_search #####

source("code/03_origin_search/00_interpolation_and_origin_search_settings.R")
cluster_up("data/origin_search/default_kernel.RData")
cluster_up("data/origin_search/retrospection_distance.RData")

source("code/03_origin_search/01_interpolation_for_selected_timeslices.R")
source("code/03_origin_search/02_interpolation_at_specific_places_median_age+one_kernel_setting.R")
source("code/03_origin_search/03_interpolation_and_search_for_selected_individuals.R")

#source("code/03_origin_search/04_origin_search_pipeline-median_age+one_kernel_setting.R")

cluster_qsub_script("code/03_origin_search/sge_origin_search.sh")
cluster_down("data/origin_search/age_resampling+one_kernel_setting")
source("code/03_origin_search/05b_merge_runs.R")

source("code/03_origin_search/06_moving_window.R")
