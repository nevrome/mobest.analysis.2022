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

bash("./singularity_build_sif.sh")
up("singularity_mobest.sif")

#### 01_poseidon_data_preparation ####

bash("./code/01_poseidon_data_preparation/00_fetch_aadr.sh")
source("./code/01_poseidon_data_preparation/00_prepare_plot_reference_data.R")
source("./code/01_poseidon_data_preparation/00_prepare_spatial_data.R")
source("./code/01_poseidon_data_preparation/01_janno_filter_for_relevant_individuals.R")
bash("./code/01_poseidon_data_preparation/02_pre_identicals_filter_poseidon_extract.sh")
bash("code/01_poseidon_data_preparation/03_distance_plink.sh")
source("code/01_poseidon_data_preparation/04_filter_by_genetic_distance.R")
bash("./code/01_poseidon_data_preparation/05_poseidon_extract.sh")
bash("code/01_poseidon_data_preparation/06_mds_plink.sh")
source("./code/01_poseidon_data_preparation/07_prepare_final_dataset.R")

up("data/spatial/")
up("data/poseidon_data/janno_final.RData")

#### 02_parameter_estimation ####

source("code/02_parameter_estimation/variogram_experiments/variogram_calculation.R")

singularity_qsub("Rscript code/02_parameter_estimation/laGP_maximum_likelihood_estimation/anisotropic_mle.R")
singularity_qsub("Rscript code/02_parameter_estimation/laGP_maximum_likelihood_estimation/isotropic_mle.R")
qsub("code/02_parameter_estimation/crossvalidation/sge_parameter_exploration.sh")
# wait until cluster run ready
down("data/parameter_exploration/mle/mlesep_out.RData")
down("data/parameter_exploration/mle/mle_out.RData")
down("data/parameter_exploration/crossvalidation/")

source("code/02_parameter_estimation/crossvalidation/modify_crossvalidation_results.R")

#### 03_origin_search ####

source("code/03_origin_search/00_interpolation_and_origin_search_settings.R")
up("data/origin_search/default_kernel.RData")
up("data/origin_search/retrospection_distance.RData")

source("code/03_origin_search/01_interpolation_for_selected_timeslices.R")
source("code/03_origin_search/02_interpolation_at_specific_places_median_age+one_kernel_setting.R")
source("code/03_origin_search/03_interpolation_and_search_for_selected_individuals.R")

#source("code/03_origin_search/04_origin_search_pipeline-median_age+one_kernel_setting.R")

qsub("code/03_origin_search/05b_sge_origin_search.sh")
down("data/origin_search/age_resampling+one_kernel_setting/")
source("code/03_origin_search/06_origin_search_merge_and_prep.R")

#### 06_alternative_parameter_exploration ####

# mds3
source("code/06_alternative_parameter_exploration/MDS_3_dimensions/01_interpolation_and_origin_search_settings.R")
up("data/origin_search/default_kernel_mds3.RData")
up("data/origin_search/retrospection_distance_mds3.RData")

source("code/06_alternative_parameter_exploration/MDS_3_dimensions/02_interpolation_for_selected_timeslices.R")

qsub("code/06_alternative_parameter_exploration/MDS_3_dimensions/03b_sge_origin_search.sh")
down("data/origin_search/age_resampling+one_kernel_setting/")
source("code/06_alternative_parameter_exploration/MDS_3_dimensions/04_origin_search_merge_and_prep.R")

# rearview distances
source("code/06_alternative_parameter_exploration/different_rearview_distances/01_interpolation_and_origin_search_settings.R")
up("data/origin_search/retrospection_distance_retrovar.RData")

qsub("code/06_alternative_parameter_exploration/different_rearview_distances/02b_sge_origin_search.sh")
down("data/origin_search/age_resampling+one_kernel_setting/")
source("code/06_alternative_parameter_exploration/different_rearview_distances/03_origin_search_merge_and_prep.R")

#### render plots ####

source("code/04_plot_scripts/paper/figure_1_temporal_and_spatial_distribution_of_input_data.R")
source("code/04_plot_scripts/paper/figure_2_mds.R")
source("code/04_plot_scripts/paper/figure_3_interpolation_map_matrix.R")
source("code/04_plot_scripts/paper/figure_4_genetic_distance_example_maps.R")
source("code/04_plot_scripts/paper/figure_5_mobility_curves.R")
source("code/04_plot_scripts/paper/figure_sup_1_semivariogram.R")
source("code/04_plot_scripts/paper/figure_sup_2_semivariogram_space_time.R")
source("code/04_plot_scripts/paper/figure_sup_3_semivariogram_fitting.R")
source("code/04_plot_scripts/paper/figure_sup_4_semivariogram_nugget.R")
source("code/04_plot_scripts/paper/figure_sup_5_mle_anisotropic.R")
source("code/04_plot_scripts/paper/figure_sup_6_kernel_size_meaning.R")
source("code/04_plot_scripts/paper/figure_sup_7_mle_isotropic.R")
source("code/04_plot_scripts/paper/figure_sup_8_crossvalidation_prediction_accuracy.R")
source("code/04_plot_scripts/paper/figure_sup_9_crossvalidation_rasters.R")
source("code/04_plot_scripts/paper/figure_sup_11_timepillars.R")
source("code/04_plot_scripts/paper/figure_sup_12_mean_direction_windrose_matrix.R")
source("code/04_plot_scripts/paper/figure_sup_13_distance_fraction_curves.R")
source("code/04_plot_scripts/paper/figure_sup_14_mds_mds3.R")
source("code/04_plot_scripts/paper/figure_sup_15_distance_correlation_mds3.R")
source("code/04_plot_scripts/paper/figure_sup_16_semivariogram_nugget_mds3.R")
source("code/04_plot_scripts/paper/figure_sup_17_crossvalidation_rasters_mds3.R")
source("code/04_plot_scripts/paper/figure_sup_18_interpolation_map_matrix_mds3.R")
source("code/04_plot_scripts/paper/figure_sup_19_mobility_curves_mds3.R")
source("code/04_plot_scripts/paper/figure_sup_20_mobility_curves_retro_low.R")
source("code/04_plot_scripts/paper/figure_sup_21_mobility_curves_retro_high.R")

# purrr::walk(
#   list.files("code/04_plot_scripts/paper", pattern = "\\.R", full.names = T),
#   \(x) {
#     message(x)
#     source(x)
#     rm(list = ls())
#   }
# )
