#!/bin/bash

#SBATCH -p short                                          # The queue or 'partition' you want to submit to
#SBATCH -c 12                                             # number of CPUs
#SBATCH --mem=20G                                         # memory pool for all cores
#SBATCH -o /projects1/coest_mobility/log/%j.out           # STDOUT (the standard output stream) into file <JOB_NUMBER>.out
#SBATCH -e /projects1/coest_mobility/log/%j.err           # STDERR (the output stream for errors) into file <JOB_NUMBER>.err
#SBATCH --array 0-9%5
#SBATCH -J "cross"

# parameters
g_to_explore=(0.005 0.01)   #(0.005 0.01 0.05 0.1)
dt_to_explore=(100 200 300 400 500) #($(seq 200 20 600))

jobs=$((${#g_to_explore[@]}*${#dt_to_explore[@]}))

echo Number of jobs: $jobs

date 

gs=()
dts=()

# paramter permutations
for g in "${g_to_explore[@]}"
do
        for dt in "${dt_to_explore[@]}"
        do
                gs+=($g)
                dts+=($dt)
	done
done

current_g=${gs[${SLURM_ARRAY_TASK_ID}]}
current_dt=${dts[${SLURM_ARRAY_TASK_ID}]}

singularity exec --bind=/projects1 ~/singularity/singularity_images/nevrome_coest/nevrome_coest.sif Rscript R/crossvalidation/interpolation_performance_test.R ${SLURM_ARRAY_TASK_ID} ${current_dt} ${current_g}

date
 
exit 0

