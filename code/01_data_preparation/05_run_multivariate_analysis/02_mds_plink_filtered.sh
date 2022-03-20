#!/bin/bash

cd data/genotype_data

# generate general pairwise stats
plink1.9 \
  --bfile snp_subsets/filtered_snp_selection/filtered_snp_selection \
  --genome \
  --out multivariate_analysis/MDS_filtered_snp_selection/pairwise_stats

# calculate mds
plink1.9 \
  --bfile snp_subsets/filtered_snp_selection/filtered_snp_selection \
  --cluster --mds-plot 10 \
  --read-genome multivariate_analysis/MDS_filtered_snp_selection/pairwise_stats.genome \
  --out multivariate_analysis/MDS_filtered_snp_selection/mds
