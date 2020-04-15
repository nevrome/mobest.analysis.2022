#!/bin/bash

jobcount=0
simuljobs=4
simuljobcount=0
gstotal=(0.001 0.005) #0.005 0.01 0.05 0.1
dtstotal=(20 40 60 80 100) #{20..300..20}
jobs=${#gstotal[@]}*${#dtstotal[@]} 
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
				echo "bash ~/singularity/slurm_nevrome_coest.sh short 8 10 --wait R/crossvalidation/interpolation_performance_test.R $jobcount ${dts[$i]} ${gs[$i]}"
				(( jobcount=jobcount+1 ))
			done
                        for (( i=0; i<simuljobs; i++ ))
                        do
                                unset -v 'gs[$i]'
				unset -v 'dts[$i]'
                        done
			(( simuljobcount=0 ))
			wait
		fi

	done
done

#do rest

for (( i=0; i<${#gs[@]}; i++ ))
do
	
	echo "bash ~/singularity/slurm_nevrome_coest.sh short 8 10 --wait R/crossvalidation/interpolation_performance_test.R $jobcount ${dts[$i]} ${gs[$i]}"
	(( jobcount=jobcount+1 ))
done
