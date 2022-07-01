#!/bin/bash

cd data/genotype_data

# generate general pairwise stats
plink1.9 \
  --bfile snp_subsets/unfiltered_snp_selection/unfiltered_snp_selection \
  --genome \
  --out multivariate_analysis/MDS_unfiltered_snp_selection/pairwise_stats

# calculate mds 
# (in the plink mds implementation there is no difference in the result for C1 and 
# C2 with --mds-plot 2 or --mds-plot X. So it's sufficient to call --mds-plot 10 once
# and then select the desired dimensions later)
plink1.9 \
  --bfile snp_subsets/unfiltered_snp_selection/unfiltered_snp_selection \
  --cluster --mds-plot 10 \
  --read-genome multivariate_analysis/MDS_unfiltered_snp_selection/pairwise_stats.genome \
  --out multivariate_analysis/MDS_unfiltered_snp_selection/mds
