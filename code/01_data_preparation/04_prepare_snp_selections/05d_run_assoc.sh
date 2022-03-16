#!/bin/bash

cd data/genotype_data/clean

plink1.9 \
  --bfile ../clean/clean1 \
  --assoc \
  --out clean1
