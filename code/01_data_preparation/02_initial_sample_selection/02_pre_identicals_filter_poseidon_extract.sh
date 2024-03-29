#!/bin/bash

rm -r data/genotype_data/initial_selection

trident forge \
  --forgeFile code/01_data_preparation/ind_list_initial_selection.txt \
  -p data/genotype_data/aadrv50_1240K/aadr_1240K.geno \
  -o data/genotype_data/initial_selection \
  --onlyGeno
