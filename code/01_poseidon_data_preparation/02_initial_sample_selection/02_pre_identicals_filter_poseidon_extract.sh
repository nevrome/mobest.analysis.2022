#!/bin/bash

rm -r data/genotype_data/poseidon_extracted_pre_identicals_filter

trident forge \
  --forgeFile code/01_genotype_data_preparation/pre_identicals_filter_ind_list.txt \
  -d data/genotype_data/aadrv50 \
  -o data/genotype_data/poseidon_extracted_pre_identicals_filter
