# qsub -b y -cwd -q archgen.q -pe smp 8 -l h_vmem=200G -now n -V -j y -o ~/log -N experiment singularity exec --bind=/mnt/archgen/users/schmid singularity_mobest.sif Rscript code/01_data_preparation/05_run_multivariate_analysis/07_pca_smartsnp_projected_unfiltered.R

data_path <- file.path(
  "data/genotype_data/snp_subsets/unfiltered_snp_selection_with_modern_reference_pops",
  "unfiltered_snp_selection_with_modern_reference_pops"
)

nr_moderns <- as.integer(strsplit(system("wc -l data/genotype_data/aadrv50_1240K_HO_Western_Eurasia_modern/aadrv50_1240K_HO_Western_Eurasia_modern.fam", intern = TRUE), " ")[[1]][1])

ind <- readr::read_tsv(
  paste0(data_path, ".ind"),
  col_names = FALSE
)

pca_out <- smartsnp::smart_pca(
  paste0(data_path, ".geno"),
  sample_group = ind$X1,
  sample_project = (nr_moderns + 1):nrow(ind),
  missing_impute = "mean",
  pc_axes = 10,
  pc_project = 1:10
)

save(
  pca_out,
  file = "data/genotype_data/multivariate_analysis/PCA_projected_unfiltered_snp_selection/pca_out.RData"
)
