#!/bin/bash

cd data/poseidon_data/mds

# pruning
plink1.9 --bfile ../poseidon_extracted/poseidon_extracted --exclude ../../../code/01_poseidon_data_preparation/myrange.txt --range --maf --make-bed --out poseidon_extracted.pruned

# generate general pairwise stats
plink1.9 --bfile poseidon_extracted.pruned --genome --out poseidon_extracted.pruned

# calculate mds 
# (in the plink mds implementation there is no difference in the result for C1 and 
# C2 with --mds-plot 2 or --mds-plot 3. So it's sufficient to call --mds-plot 3 once
# and then select the desired dimensions later)
plink1.9 --bfile poseidon_extracted.pruned --cluster --mds-plot 3 --read-genome poseidon_extracted.pruned.genome --out mds
