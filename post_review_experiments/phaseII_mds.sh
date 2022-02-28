#!/bin/bash

rm -r post_review_experiments/data/phaseII_package

trident forge \
  --forgeFile post_review_experiments/data/ind_list_phaseII.txt \
  -d data/poseidon_data/poseidon_extracted \
  -o post_review_experiments/data/phaseII_package \
  -n phaseII_package

#!/bin/bash

cd post_review_experiments/data/phaseII_package

# pruning
plink1.9 --bfile phaseII_package --exclude ../../../code/01_poseidon_data_preparation/myrange.txt --range --maf --make-bed --out phaseII.pruned

# pairwise distances
plink1.9 --bfile phaseII.pruned --distance square 1-ibs

# generate general pairwise stats
plink1.9 --bfile phaseII.pruned --genome --out phaseII.pruned

# calculate mds 
# (in the plink mds implementation there is no difference in the result for C1 and 
# C2 with --mds-plot 2 or --mds-plot 3. So it's sufficient to call --mds-plot 3 once
# and then select the desired dimensions later)
plink1.9 --bfile phaseII.pruned --cluster --mds-plot 10 --read-genome phaseII.pruned.genome --out phaseII.pruned.mds
