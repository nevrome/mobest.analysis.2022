#!/bin/bash

trident init \
  --inFormat PLINK \
  --snpSet Other \
  --genoFile data/poseidon_data/clean/clean1.bed \
  --snpFile data/poseidon_data/clean/clean1.bim \
  --indFile data/poseidon_data/clean/clean1.fam \
  --minimal \
  -o data/poseidon_data/clean/huhu

trident forge \
  -f "" \
  -d data/poseidon_data/clean/huhu \
  -o data/poseidon_data/clean/bubu \
  --selectSnps data/poseidon_data/clean/filter.bim
  
  