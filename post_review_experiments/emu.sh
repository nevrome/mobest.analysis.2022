#!/bin/bash

emu \
  --plink data/poseidon_data/poseidon_extracted/poseidon_extracted \
  --n_eig 10 \
  --n_out 3 \
  --threads 8 \
  --out emu_out.txt \
  --loadings emu_loadings.txt
