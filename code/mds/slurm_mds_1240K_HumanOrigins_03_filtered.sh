#!/bin/bash

#SBATCH -p short                                          # The queue or 'partition' you want to submit to
#SBATCH -c 10                                             # number of CPUs
#SBATCH --mem=100G                                        # memory pool for all cores
#SBATCH -o /projects1/coest_mobility/log/%j.out           # STDOUT (the standard output stream) into file <JOB_NUMBER>.out
#SBATCH -e /projects1/coest_mobility/log/%j.err           # STDERR (the output stream for errors) into file <JOB_NUMBER>.err
#SBATCH -J "mds"

date 

cd /projects1/coest_mobility/coest.interpol.2020/data/mds

# remove outlier individuals
plink --file 1240K_HumanOrigins --remove ../../code/mds/outlier_individuals.txt --make-bed --out 1240K_HumanOrigins.filtered

# pruning
plink --bfile 1240K_HumanOrigins.filtered --exclude ../../code/mds/myrange.txt --range --maf --make-bed --out 1240K_HumanOrigins.filtered.pruned

# generate general pairwise stats
plink --bfile 1240K_HumanOrigins.filtered.pruned --genome --out 1240K_HumanOrigins.filtered.pruned

# create mds table
plink --bfile 1240K_HumanOrigins.filtered.pruned --cluster --mds-plot 4 --read-genome 1240K_HumanOrigins.filtered.pruned.genome --out 1240K_HumanOrigins.filtered.pruned

date

exit 0
