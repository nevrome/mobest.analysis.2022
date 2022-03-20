# qsub -b y -cwd -q archgen.q -pe smp 8 -l h_vmem=200G -now n -V -j y -o ~/log -N experiment singularity exec --bind=/mnt/archgen/users/schmid singularity_mobest.sif Rscript code/01_data_preparation/05_run_multivariate_analysis/04_pca_smartsnp_filtered.R

fam <- readr::read_tsv(
  "data/genotype_data/snp_subsets/filtered_snp_selection/filtered_snp_selection.fam",
  col_names = FALSE
)

pca_out <- smartsnp::smart_pca(
  "data/genotype_data/snp_subsets/filtered_snp_selection/filtered_snp_selection.geno",
  sample_group = seq_len(nrow(fam)),
  missing_impute = "mean",
  pc_axes = 10
)

save(
  pca_out,
  file = "data/genotype_data/multivariate_analysis/PCA_filtered_snp_selection/pca_out.RData"
)
