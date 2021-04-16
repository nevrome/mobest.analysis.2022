source("code/technical_helpers.R")

source("code/02_parameter_estimation/variogram_experiments/variogram_calculation.R")

cluster_run("cd /mnt/archgen/users/schmid/mobest.analysis.2020 && ../singularity/sge_nevrome_mobest.sh 16 50 code/02_parameter_estimation/laGP_maximum_likelihood_estimation/isotropic_mle.R")
cluster_run("cd /mnt/archgen/users/schmid/mobest.analysis.2020 && ../singularity/sge_nevrome_mobest.sh 16 50 code/02_parameter_estimation/laGP_maximum_likelihood_estimation/anisotropic_mle.R")
cluster_qsub_script("code/02_parameter_estimation/crossvalidation/sge_parameter_exploration.sh")

cluster_down("data/parameter_exploration/mle/mle_out.RData")
cluster_down("data/parameter_exploration/mle/mlesep_out.RData")
cluster_down("data/parameter_exploration/crossvalidation")

source("code/02_parameter_estimation/crossvalidation/modify_crossvalidation_results.R")

