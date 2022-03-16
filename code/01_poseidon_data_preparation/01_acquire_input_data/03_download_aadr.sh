#!/bin/bash

wget https://reichdata.hms.harvard.edu/pub/datasets/amh_repo/curated_releases/V50/V50.0/SHARE/public.dir/v50.0_1240K_public.tar -O data/genotype_data/aadrv50/tar_archive.tar
tar -xvf data/genotype_data/aadrv50/tar_archive.tar -C data/genotype_data/aadrv50

cat > data/genotype_data/aadrv50/convertf_parfile <<EOF
genotypename: data/genotype_data/aadrv50/v50.0_1240k_public.geno
snpname: data/genotype_data/aadrv50/v50.0_1240k_public.snp
indivname: data/genotype_data/aadrv50/v50.0_1240k_public.ind
outputformat: EIGENSTRAT
genotypeoutname: data/genotype_data/aadrv50/aadr_eig.geno
snpoutname: data/genotype_data/aadrv50/aadr_eig.snp
indivoutname: data/genotype_data/aadrv50/aadr_eig.ind
EOF

convertf -p data/genotype_data/aadrv50/convertf_parfile
