#!/bin/bash

rm -r data/genotype_data/poseidon_extracted

trident forge \
  --forgeFile code/01_data_preparation/ind_list.txt \
  -d data/genotype_data/aadrv50 \
  -o data/genotype_data/poseidon_extracted
