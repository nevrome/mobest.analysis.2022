#!/bin/bash

rm -r data/poseidon_data/poseidon_extracted_pre_identicals_filter

trident forge \
  --forgeFile data/poseidon_data/identical_filter/pre_identicals_filter_ind_list.txt \
  -d data/poseidon_data/poseidon_full \
  -n poseidon_extracted_pre_identicals_filter \
  -o data/poseidon_data/poseidon_extracted_pre_identicals_filter
