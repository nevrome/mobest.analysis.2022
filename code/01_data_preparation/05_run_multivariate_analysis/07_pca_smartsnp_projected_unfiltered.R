# qsub -b y -cwd -q archgen.q -pe smp 8 -l h_vmem=200G -now n -V -j y -o ~/log -N experiment singularity exec --bind=/mnt/archgen/users/schmid singularity_mobest.sif Rscript code/01_data_preparation/05_run_multivariate_analysis/07_pca_smartsnp_projected_unfiltered.R

cur_path <- file.path(
  "data/genotype_data/snp_subsets/unfiltered_snp_selection_with_modern_reference_pops",
  "unfiltered_snp_selection_with_modern_reference_pops"
)

fam <- readr::read_tsv(
  paste0(cur_path, ".fam"),
  col_names = FALSE
)

pca_out <- smartsnp::smart_pca(
  paste0(cur_path, "geno"),
  sample_group = fam$X1,
  missing_impute = "mean",
  pc_axes = 10,
  pc_project = 1:10
)

save(
  pca_out,
  file = "data/genotype_data/multivariate_analysis/PCA_projected_unfiltered_snp_selection/pca_out.RData"
)
