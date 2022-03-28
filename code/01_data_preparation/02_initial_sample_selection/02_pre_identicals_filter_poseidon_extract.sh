#!/bin/bash

rm -r data/genotype_data/initial_selection

trident forge \
  --forgeFile code/01_data_preparation/ind_list_initial_selection.txt \
  -d data/genotype_data/aadrv50_1240K \
  -o data/genotype_data/initial_selection
