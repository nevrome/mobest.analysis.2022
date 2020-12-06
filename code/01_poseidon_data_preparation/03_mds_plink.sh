#!/bin/bash

#SBATCH -p long                                           # The queue or 'partition' you want to submit to
#SBATCH -c 32                                             # number of CPUs
#SBATCH --mem=100G                                        # memory pool for all cores
#SBATCH -o /projects1/coest_mobility/log/%j.out           # STDOUT (the standard output stream) into file <JOB_NUMBER>.out
#SBATCH -e /projects1/coest_mobility/log/%j.err           # STDERR (the output stream for errors) into file <JOB_NUMBER>.err
#SBATCH -J "mds"

date 

cd /projects1/coest_mobility/mobest.analysis.2020/data/poseidon_data/mds

# pruning
plink --bfile ../poseidon_extracted/poseidon_extracted --exclude ../../../code/01_poseidon_data_preparation/myrange.txt --range --maf --make-bed --out poseidon_extracted.pruned

# generate general pairwise stats
plink --bfile poseidon_extracted.pruned --genome --out poseidon_extracted.pruned

# create mds table
plink --bfile poseidon_extracted.pruned --cluster --mds-plot 2 --read-genome poseidon_extracted.pruned.genome --out poseidon_extracted.pruned

date

exit 0

