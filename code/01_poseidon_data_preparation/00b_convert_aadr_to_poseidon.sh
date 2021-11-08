#!/bin/bash

# this is necessary, because Shake creates directories, but trident can not work if there already is one
rm -r data/poseidon_data/aadrv50/aadr_poseidon
trident init --inFormat EIGENSTRAT --snpSet 1240K --genoFile data/poseidon_data/aadrv50/aadr_eig.geno --snpFile data/poseidon_data/aadrv50/aadr_eig.snp --indFile data/poseidon_data/aadrv50/aadr_eig.ind --minimal -o data/poseidon_data/aadrv50/aadr_poseidon
