#!/bin/bash

i=0
for g in 0.001 0.005 0.01 0.05 0.1
do
	for dt in {20..300..20}
	do
		echo "bash ~/singularity/slurm_nevrome_coest.sh short 8 10 R/crossvalidation/interpolation_performance_test.R $i $dt $g"
		((i=i+1))
	done
done

