#!/bin/bash

cd data/genotype_data/identical_filter

# pruning
plink1.9 --bfile ../poseidon_extracted_pre_identicals_filter/poseidon_extracted_pre_identicals_filter --exclude ../../../code/01_data_preparation/myrange.txt --range --maf --make-bed --out poseidon_extracted_pre_identicals_filter.pruned

# pairwise distances
plink1.9 --bfile poseidon_extracted_pre_identicals_filter.pruned --distance square 1-ibs
