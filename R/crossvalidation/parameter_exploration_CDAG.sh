#!/bin/bash

jobcount=0
simuljobs=4
sleeptime=10  #10*60
simuljobcount=0

gstotal=(0.001 0.005) #0.005 0.01 0.05 0.1
dtstotal=(20 40 60) # 80 100) #{20..300..20}
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
				bash ~/singularity/slurm_nevrome_coest.sh short 8 10 R/crossvalidation/interpolation_performance_test.R $jobcount ${dts[$i]} ${gs[$i]}
				(( jobcount=jobcount+1 ))
				echo Running job $jobcount of $jobs 
			done
                        for (( i=0; i<simuljobs; i++ ))
                        do
                                unset -v 'gs[$i]'
				unset -v 'dts[$i]'
                        done
			(( simuljobcount=0 ))

			sleep $sleeptime
		fi

	done
done

#do the rest

for (( i=0; i<${#gs[@]}; i++ ))
do
	
	bash ~/singularity/slurm_nevrome_coest.sh short 8 10 R/crossvalidation/interpolation_performance_test.R $jobcount ${dts[$i]} ${gs[$i]}
	(( jobcount=jobcount+1 ))
	echo Running job $jobcount of $jobs
done
