#!/bin/bash

#SBATCH -p short                                          # The queue or 'partition' you want to submit to
#SBATCH -c 10                                             # number of CPUs
#SBATCH --mem=10G                                         # memory pool for all cores
#SBATCH -o /projects1/coest_mobility/log/%j.out           # STDOUT (the standard output stream) into file <JOB_NUMBER>.out
#SBATCH -e /projects1/coest_mobility/log/%j.err           # STDERR (the output stream for errors) into file <JOB_NUMBER>.err
#SBATCH --array 0-9%5
#SBATCH -J "mob"

date 

singularity exec --bind=/projects1 ~/singularity/singularity_images/nevrome_coest/nevrome_coest.sif Rscript code/mobility_estimation/06_interpolation_age_resampling.R ${SLURM_ARRAY_TASK_ID}

date
 
exit 0

