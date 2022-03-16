#!/bin/bash

cd data/genotype_data/clean

trident init \
  --inFormat PLINK \
  --snpSet Other \
  --genoFile clean1.bed \
  --snpFile clean1.bim \
  --indFile clean1.fam \
  --minimal \
  -o huhu

trident forge \
  -f "" \
  -d huhu \
  -o bubu \
  --selectSnps filter.bim
  
  
