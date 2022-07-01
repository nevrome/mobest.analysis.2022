#!/bin/bash

cd data/genotype_data/snp_subsets

# --exclude: remove SNPs in high LD according to list in myrange.txt
# --maf: filter out all variants with minor allele frequency below the provided threshold (default 0.01)
plink1.9 \
  --bfile ../remove_related_individuals/remove_related_selection/remove_related_selection \
  --exclude range ../../../code/01_data_preparation/myrange.txt \
  --maf 0.05 \
  --make-bed --out purified


