#!/bin/bash

task () {
	bash ~/singularity/slurm_nevrome_coest.sh short 12 20 R/crossvalidation/interpolation_performance_test.R $1 $2 $3
}

jobcount=0
simuljobs=4
simuljobcount=0

gstotal=(0.001 0.005 0.01 0.05 0.1)
dtstotal=($(seq 20 20 500)) #(20 40 60) # 80 100) #{20..300..20}

jobs=$((${#gstotal[@]}*${#dtstotal[@]}))

gs=()
dts=() 

for g in "${gstotal[@]}"
do
	for dt in "${dtstotal[@]}"
	do
		gs+=($g)
		dts+=($dt)
		(( simuljobcount=simuljobcount+1 ))

		if (( simuljobcount == simuljobs ))
		then
			for (( i=0; i<simuljobs; i++ ))
			do
				echo Running job $jobcount of $jobs
				if (( i == simuljobs-1 ))
				then
					export SBATCH_WAIT=1
					task $jobcount ${dts[$i]} ${gs[$i]}
				else
					unset SBATCH_WAIT
					task $jobcount ${dts[$i]} ${gs[$i]}
				fi
				(( jobcount=jobcount+1 ))
			done
                        for (( i=0; i<simuljobs; i++ ))
                        do
                                unset -v 'gs[$i]'
				unset -v 'dts[$i]'
                        done
			(( simuljobcount=0 ))
		fi

	done
done

#do the rest

for (( i=0; i<${#gs[@]}; i++ ))
do
	echo Running job $jobcount of $jobs
	unset SBATCH_WAIT
	task $jobcount ${dts[$i]} ${gs[$i]}
	(( jobcount=jobcount+1 ))
done
