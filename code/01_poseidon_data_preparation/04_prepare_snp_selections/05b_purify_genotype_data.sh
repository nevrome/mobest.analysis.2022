#!/bin/bash

cd data/poseidon_data/clean

# --exclude: remove SNPs in high LD according to list in myrange.txt
# --maf: filter out all variants with minor allele frequency below the provided threshold (default 0.01)
plink1.9 \
  --bfile ../poseidon_extracted/poseidon_extracted \
  --exclude range ../../../code/01_poseidon_data_preparation/myrange.txt \
  --maf 0.05 \
  --make-bed --out purified


