#!/bin/bash

rm -r data/poseidon_data/poseidon_extracted

trident forge \
  --forgeFile code/01_poseidon_data_preparation/ind_list.txt \
  -d data/poseidon_data/poseidon_extracted_pre_identicals_filter \
  -n poseidon_extracted \
  -o data/poseidon_data/poseidon_extracted

# qsub -N trident -b y -cwd -pe smp 1 -l h_vmem=4G ./code/01_poseidon_data_preparation/02_poseidon_extract.sh