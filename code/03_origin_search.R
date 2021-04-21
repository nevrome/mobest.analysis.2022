source("code/technical_helpers.R")

source("code/03_origin_search/01_interpolation_at_specific_places_median_age+one_kernel_setting.R")

source("code/03_origin_search/02_origin_search_pipeline-median_age+one_kernel_setting.R")

cluster_qsub_script("code/03_origin_search/sge_origin_search.sh")
source("code/03_origin_search/03b_merge_runs.R")
source("code/03_origin_search/04_moving_window.R")
