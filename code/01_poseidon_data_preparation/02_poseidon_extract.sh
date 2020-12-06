#!/bin/bash

rm -r /projects1/coest_mobility/mobest.analysis.2020/data/poseidon_data/poseidon_extracted

trident forge \
  --forgeFile /projects1/coest_mobility/mobest.analysis.2020/code/01_poseidon_data_preparation/ind_list.txt \
  -d /projects1/poseidon/repo/ancient \
  -n poseidon_extracted \
  -o /projects1/coest_mobility/mobest.analysis.2020/data/poseidon_data/poseidon_extracted

# so far poseidon does not support PLINK-Binary output
convertf -p ../../../code/01_poseidon_data_preparation/par.extracted_eigenstrat_to_plinkbinary

# sbatch -p long -c 1 --mem=20G -J "trident" --wrap="/projects1/coest_mobility/mobest.analysis.2020/code/01_poseidon_data_preparation/02_poseidon_extract.sh"