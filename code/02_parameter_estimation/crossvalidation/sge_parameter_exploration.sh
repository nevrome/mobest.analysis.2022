#!/bin/bash
qsub <<EOT
#!/bin/bash

#$ -S /bin/bash #defines bash as the shell for execution
#$ -N cross #Name of the command that will be listed in the queue
#$ -cwd #change to current directory
#$ -j y #join error and standard output in one file, no error file will be written
#$ -o ~/log #standard output file or directory (joined with error because of -j y)
#$ -q archgen.q #queue
#$ -pe make 5 #needs X CPU cores
#$ -l h_vmem=10G #request XGb of memory
#$ -V # load personal profile
#$ -t 1:400 # array job length
#$ -tc 10 # number of concurrently running tasks in array


# parameters
g_to_explore=(0.08)
dt_to_explore=($(seq 100 100 2000))

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

current_g=${gs[${SGE_TASK_ID}]}
current_dt=${dts[${SGE_TASK_ID}]}

singularity exec --bind=/mnt/archgen/users/schmid ../singularity/images/nevrome_mobest/nevrome_mobest.sif Rscript code/02_parameter_estimation/crossvalidation/crossvalidation.R ${SGE_TASK_ID} ${current_dt} ${current_g}

date
 
exit 0
EOT