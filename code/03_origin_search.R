source("code/technical_helpers.R")

source("code/03_origin_search/01_interpolation_for_selected_timeslices.R")
source("code/03_origin_search/02_interpolation_at_specific_places_median_age+one_kernel_setting.R")
source("code/03_origin_search/03_interpolation_and_search_for_selected_individuals.R")
source("code/03_origin_search/04_origin_search_pipeline-median_age+one_kernel_setting.R")

cluster_qsub_script("code/03_origin_search/sge_origin_search.sh")
cluster_down("data/origin_search/age_resampling+one_kernel_setting")
source("code/03_origin_search/05b_merge_runs.R")

source("code/03_origin_search/06_moving_window.R")
