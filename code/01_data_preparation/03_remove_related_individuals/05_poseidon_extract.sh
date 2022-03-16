#!/bin/bash

rm -r data/genotype_data/poseidon_extracted

trident forge \
  --forgeFile code/01_data_preparation/ind_list.txt \
  -d data/genotype_data/aadrv50 \
  -o data/genotype_data/poseidon_extracted

# qsub -N trident -b y -cwd -pe smp 1 -l h_vmem=4G ./code/01_data_preparation/02_poseidon_extract.sh
