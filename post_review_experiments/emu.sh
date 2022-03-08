#!/bin/bash

# qsub -b y -cwd -q archgen.q -pe smp 8  -l h_vmem=100G -now n -V -j y -o ~/log -N experiment singularity exec --bind=/mnt/archgen/users/schmid singularity_mobest.sif ./post_review_experiments/emu.sh

emu \
  --plink data/poseidon_data/poseidon_extracted/poseidon_extracted \
  --n_eig 10 \
  --n_out 3 \
  --threads 8 \
  --out emu_out.txt \
  --loadings emu_loadings.txt
