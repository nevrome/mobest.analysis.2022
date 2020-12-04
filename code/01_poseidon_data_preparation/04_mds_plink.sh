#!/bin/bash

#SBATCH -p short                                          # The queue or 'partition' you want to submit to
#SBATCH -c 10                                             # number of CPUs
#SBATCH --mem=100G                                        # memory pool for all cores
#SBATCH -o /projects1/coest_mobility/log/%j.out           # STDOUT (the standard output stream) into file <JOB_NUMBER>.out
#SBATCH -e /projects1/coest_mobility/log/%j.err           # STDERR (the output stream for errors) into file <JOB_NUMBER>.err
#SBATCH -J "mds"

date 

cd /projects1/coest_mobility/coest.interpol.2020/data/poseidon_data/mds

# pruning
plink --bfile ../poseidon_extracted/poseidon2_extracted --exclude ../../../code/poseidon_data_preparation/myrange.txt --range --maf --make-bed --out poseidon2_extracted.pruned

# generate general pairwise stats
plink --bfile poseidon2_extracted.pruned --genome --out poseidon2_extracted.pruned

# create mds table
plink --bfile poseidon2_extracted.pruned --cluster --mds-plot 2 --read-genome poseidon2_extracted.pruned.genome --out poseidon2_extracted.pruned

date

exit 0
