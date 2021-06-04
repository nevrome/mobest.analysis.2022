#!/bin/bash

rm -r data/poseidon_data/poseidon_extracted_pre_identicals_filter

trident forge \
  --forgeFile code/01_poseidon_data_preparation/pre_identicals_filter_ind_list.txt \
  -d data/poseidon_data/poseidon_full \
  -n poseidon_extracted_pre_identicals_filter \
  -o data/poseidon_data/poseidon_extracted_pre_identicals_filter
