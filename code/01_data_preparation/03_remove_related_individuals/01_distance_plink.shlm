#!/bin/bash

cd data/genotype_data/remove_related_individuals

# pruning
plink1.9 \
  --bfile ../initial_selection/initial_selection \
  --exclude ../../../code/01_data_preparation/myrange.txt --range \
  --maf \
  --make-bed \
  --out initial_selection.cleaned

# pairwise distances
plink1.9 \
  --bfile initial_selection.cleaned \
  --distance square 1-ibs
