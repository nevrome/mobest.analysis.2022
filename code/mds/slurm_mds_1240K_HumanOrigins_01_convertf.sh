#!/bin/bash

#SBATCH -p short                                          # The queue or 'partition' you want to submit to
#SBATCH -c 10                                             # number of CPUs
#SBATCH --mem=100G                                        # memory pool for all cores
#SBATCH -o /projects1/coest_mobility/log/%j.out           # STDOUT (the standard output stream) into file <JOB_NUMBER>.out
#SBATCH -e /projects1/coest_mobility/log/%j.err           # STDERR (the output stream for errors) into file <JOB_NUMBER>.err
#SBATCH -J "mds"

date 

cd /projects1/coest_mobility/coest.interpol.2020

# convert eigenstrat to ped and filter to old populations
convertf -p code/mds/mds_convertf.par

cd data/mds

# rename .pedsnp to .map
cp 1240K_HumanOrigins.pedsnp 1240K_HumanOrigins.map

date

exit 0
