#!/bin/bash

cd data/genotype_data/snp_subsets

trident init \
  --inFormat PLINK \
  -p purified.bed \
  -o unfiltered_snp_selection_pre_ind_correction

trident forge \
  -f "" \
  -d unfiltered_snp_selection_pre_ind_correction \
  -o filtered_snp_selection_pre_ind_correction \
  --selectSnps capture_shotgun_filter.bim
