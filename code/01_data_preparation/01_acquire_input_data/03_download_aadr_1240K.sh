#!/bin/bash

wget https://reichdata.hms.harvard.edu/pub/datasets/amh_repo/curated_releases/V50/V50.0/SHARE/public.dir/v50.0_1240K_public.tar -O data/genotype_data/aadrv50_1240K/tar_archive.tar
tar -xvf data/genotype_data/aadrv50_1240K/tar_archive.tar -C data/genotype_data/aadrv50_1240K

cat > data/genotype_data/aadrv50_1240K/convertf_parfile <<EOF
genotypename: data/genotype_data/aadrv50_1240K/v50.0_1240k_public.geno
snpname: data/genotype_data/aadrv50_1240K/v50.0_1240k_public.snp
indivname: data/genotype_data/aadrv50_1240K/v50.0_1240k_public.ind
outputformat: EIGENSTRAT
genotypeoutname: data/genotype_data/aadrv50_1240K/aadr_1240K.geno
snpoutname: data/genotype_data/aadrv50_1240K/aadr_1240K.snp
indivoutname: data/genotype_data/aadrv50_1240K/aadr_1240K.ind
EOF

convertf -p data/genotype_data/aadrv50_1240K/convertf_parfile

cat > data/genotype_data/aadrv50_1240K/POSEIDON.yml <<EOF
poseidonVersion: 2.5.0
title: aadrv50_1240K
contributor:
  - name: John Doe
    email: john@doe.net
packageVersion: 0.1.0
genotypeData:
  format: EIGENSTRAT
  genoFile: aadr_1240K_HO.geno
  snpFile: aadr_1240K_HO.snp
  indFile: aadr_1240K_HO.ind
  snpSet: 1240K
jannoFile: aadr_poseidon.janno
EOF
