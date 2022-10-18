#!/bin/bash

cd data/genotype_data/snp_subsets

plink1.9 \
  --bfile purified \
  --assoc
