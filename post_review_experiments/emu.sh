#!/bin/bash

# qsub -b y -cwd -q archgen.q -pe smp 36 -l h_vmem=50G -now n -V -j y -o ~/log -N experiment singularity exec --bind=/mnt/archgen/users/schmid singularity_mobest.sif ./post_review_experiments/emu.sh
# low memory: untested

emu \
  --plink data/poseidon_data/poseidon_extracted/poseidon_extracted \
  --n_eig 3 \
  --n_out 3 \
  --maf 0.05 \
  --threads 36 \
  --out emu_out.txt \
  --loadings
