#!/bin/bash

# wget https://reichdata.hms.harvard.edu/pub/datasets/amh_repo/curated_releases/V50/V50.0/SHARE/public.dir/v50.0_1240K_public.tar -O data/poseidon_data/aadrv50/tar_archive.tar
# tar -xvf data/poseidon_data/aadrv50/tar_archive.tar -C data/poseidon_data/aadrv50

# cat > data/poseidon_data/aadrv50/convertf_parfile <<EOF
# genotypename: data/poseidon_data/aadrv50/v50.0_1240k_public.geno
# snpname: data/poseidon_data/aadrv50/v50.0_1240k_public.snp
# indivname: data/poseidon_data/aadrv50/v50.0_1240k_public.ind
# outputformat: EIGENSTRAT
# genotypeoutname: data/poseidon_data/aadrv50/aadr_plink.geno
# snpoutname: data/poseidon_data/aadrv50/aadr_plink.snp
# indivoutname: data/poseidon_data/aadrv50/aadr_plink.ind
# EOF

# convertf -p data/poseidon_data/aadrv50/convertf_parfile
trident init --inFormat EIGENSTRAT --snpSet 1240K --genoFile data/poseidon_data/aadrv50/aadr_plink.geno --snpFile data/poseidon_data/aadrv50/aadr_plink.snp --indFile data/poseidon_data/aadrv50/aadr_plink.ind -o data/poseidon_data/aadrv50/aadr_poseidon -n aadr_poseidon