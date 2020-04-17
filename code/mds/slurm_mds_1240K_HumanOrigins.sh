#!/bin/bash

#SBATCH -p short                                          # The queue or 'partition' you want to submit to
#SBATCH -c 11                                             # number of CPUs
#SBATCH --mem=20G                                         # memory pool for all cores
#SBATCH -o /projects1/coest_mobility/log/%j.out           # STDOUT (the standard output stream) into file <JOB_NUMBER>.out
#SBATCH -e /projects1/coest_mobility/log/%j.err           # STDERR (the output stream for errors) into file <JOB_NUMBER>.err
#SBATCH -J "cross"

date 

eigenstrat -> convertf + config-file (.par) + hier filtering mit option poplistname -> ped

cp Data.merged.pedsnp Data.merged.map

# pruning
plink --file Data.merged --exclude myrange.txt --range --maf --make-bed --out Data.merged.pruned

# generate general pairwise stats
plink --file Data.merged --genome

# create mds table
plink --bfile Data.merged --cluster --mds-plot 4 --read-genome Data.merged.genome

date
 
exit 0