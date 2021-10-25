#!/bin/bash

# this is necessary, because Shake creates directories, but trident can not work if there alread is one
rm -r data/poseidon_data/aadrv50/aadr_poseidon
trident init --inFormat EIGENSTRAT --snpSet 1240K --genoFile data/poseidon_data/aadrv50/aadr_eig.geno --snpFile data/poseidon_data/aadrv50/aadr_eig.snp --indFile data/poseidon_data/aadrv50/aadr_eig.ind -o data/poseidon_data/aadrv50/aadr_poseidon -n aadr_poseidon
# this will not be necessary, when poseidon has a --minimal flag:
rm data/poseidon_data/aadrv50/aadr_poseidon/aadr_poseidon.janno
