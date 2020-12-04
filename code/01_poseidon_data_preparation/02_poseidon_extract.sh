#!/bin/bash

rm -r /projects1/coest_mobility/mobest.analysis.2020/data/poseidon_data/poseidon_extracted

trident forge \
  --forgeFile /projects1/coest_mobility/mobest.analysis.2020/code/poseidon_data_preparation/ind_list.txt \
  -d /projects1/poseidon/repo/ancient \
  -n poseidon_extracted \
  -o /projects1/coest_mobility/mobest.analysis.2020/data/poseidon_data/poseidon_extracted

