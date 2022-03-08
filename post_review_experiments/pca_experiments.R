# qsub -b y -cwd -q archgen.q -pe smp 8  -l h_vmem=100G -now n -V -j y -o ~/log -N experiment singularity exec --bind=/mnt/archgen/users/schmid singularity_mobest.sif Rscript post_review_experiments/pca_experiments.R

library(magrittr)

#### projection pca ####

fam <- readr::read_tsv(
  "data/poseidon_data/poseidon_extracted/poseidon_extracted.fam",
  col_names = FALSE
  )

to_project <- fam %>%
  dplyr::mutate(
    Capture_Type = dplyr::case_when(
      grepl(".SG", X2) ~ "Shotgun",
      TRUE ~ "Capture"
    )
  ) %$%
  which(Capture_Type == "Shotgun")

pca_out <- smartsnp::smart_pca(
  "data/poseidon_data/poseidon_extracted/poseidon_extracted.geno",
  sample_group = seq_len(nrow(fam)),
  missing_impute = "mean",
  pc_axes = 3,
  sample_project = to_project,
  pc_project = c(1,2,3)
)

save(pca_out, file = "pca_experiment_project_shotgun_on_capture.RData")

#### normal pca ####

# pca_out <- smartsnp::smart_pca(
#   "data/poseidon_data/poseidon_extracted/poseidon_extracted.geno",
#   sample_group = seq_len(nrow(fam)),
#   missing_impute = "mean",
#   pc_axes = 3,
#   pc_project = 3
# )
# 
# save(pca_out, file = "pca_experiment.RData")
