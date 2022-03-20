#!/bin/bash

cd data/genotype_data

# qsub -b y -cwd -q archgen.q -pe smp 48 -l h_vmem=100G -now n -V -j y -o ~/log -N experiment singularity exec --bind=/mnt/archgen/users/schmid singularity_mobest.sif ./code/01_data_preparation/05_run_multivariate_analysis/05_emu_filtered.sh

emu \
  --plink snp_subsets/filtered_snp_selection/filtered_snp_selection \
  --n_eig 10 \
  --n_out 10 \
  --maf 0.05 \
  --threads 48 \
  --out multivariate_analysis/EMU_filtered_snp_selection/emu_out.txt \
  --loadings
