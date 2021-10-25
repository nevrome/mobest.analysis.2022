#!/bin/bash
#
#$ -S /bin/bash #defines bash as the shell for execution
#$ -N cross #Name of the command that will be listed in the queue
#$ -cwd #change to current directory
#$ -j y #join error and standard output in one file, no error file will be written
#$ -o ~/log #standard output file or directory (joined with error because of -j y)
#$ -q archgen.q #queue
#$ -pe smp 2 #needs X CPU cores
#$ -l h_vmem=5G #request XGb of memory
#$ -V # load personal profile
#$ -t 1-800 # array job length
#$ -tc 30 # number of concurrently running tasks in array


date

echo Task in Array: ${SGE_TASK_ID}

i=$((SGE_TASK_ID - 1))

echo Index: ${i}

# parameters
ds_to_explore=($(seq 100 100 2000))
dt_to_explore=($(seq 100 100 2000))
g_to_explore=(0.07 0.22)

jobs=$((${#g_to_explore[@]}*${#ds_to_explore[@]}*${#dt_to_explore[@]}))

echo Number of jobs: $jobs

dss=()
dts=()
gs=()

# parameter permutations
for g in "${g_to_explore[@]}"
do
  for ds in "${ds_to_explore[@]}"
  do
    for dt in "${dt_to_explore[@]}"
    do
      dss+=($ds)
      dts+=($dt)
      gs+=($g)
    done
  done
done

current_ds=${dss[${i}]}
current_dt=${dts[${i}]}
current_g=${gs[${i}]}

echo ds: ${current_ds}
echo dt: ${current_dt}
echo g: ${current_g}

singularity exec --bind=/mnt/archgen/users/schmid singularity_mobest.sif Rscript code/02_parameter_estimation/crossvalidation/crossvalidation.R ${i} ${current_ds} ${current_dt} ${current_g}

date

exit 0
