# qsub -sync y -b y -cwd -q archgen.q -pe smp 8  -l h_vmem=40G -now n -V -j y -o ~/log -N experiment singularity exec --bind=/mnt/archgen/users/schmid singularity_mobest.sif Rscript post_review_experiments/pca_experiment.R

pca_out <- smartsnp::smart_pca(
  "data/poseidon_data/poseidon_extracted/poseidon_extracted.geno",
  sample_group = 1:3191,
  missing_impute = "mean"
)

save(pca_out, file = "pca_experiment.RData")
