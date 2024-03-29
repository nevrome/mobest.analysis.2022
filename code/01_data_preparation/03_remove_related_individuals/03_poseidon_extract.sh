#!/bin/bash

rm -r data/genotype_data/remove_related_individuals/remove_related_selection

trident forge \
  --forgeFile code/01_data_preparation/ind_list_remove_related_selection.txt \
  -p data/genotype_data/aadrv50_1240K/aadr_1240K.geno \
  -o data/genotype_data/remove_related_individuals/remove_related_selection \
  --onlyGeno
