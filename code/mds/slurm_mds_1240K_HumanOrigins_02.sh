#!/bin/bash

#SBATCH -p short                                          # The queue or 'partition' you want to submit to
#SBATCH -c 10                                             # number of CPUs
#SBATCH --mem=100G                                        # memory pool for all cores
#SBATCH -o /projects1/coest_mobility/log/%j.out           # STDOUT (the standard output stream) into file <JOB_NUMBER>.out
#SBATCH -e /projects1/coest_mobility/log/%j.err           # STDERR (the output stream for errors) into file <JOB_NUMBER>.err
#SBATCH -J "mds"

date 

cd /projects1/coest_mobility/coest.interpol.2020/data/mds

# pruning
plink --file 1240K_HumanOrigins --exclude ../../code/mds/myrange.txt --range --maf --make-bed --out 1240K_HumanOrigins.pruned

# generate general pairwise stats
plink --bfile 1240K_HumanOrigins.pruned --genome --out 1240K_HumanOrigins.pruned

# create mds table
plink --bfile 1240K_HumanOrigins.pruned --cluster --mds-plot 4 --read-genome 1240K_HumanOrigins.pruned.genome --out 1240K_HumanOrigins.pruned

date

exit 0
