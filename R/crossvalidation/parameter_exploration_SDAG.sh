#!/bin/bash

~/singularity/slurm_nevrome_coest.sh short 64 100 R/crossvalidation/interpolation_performance_test.R 1 0.001
~/singularity/slurm_nevrome_coest.sh short 64 100 R/crossvalidation/interpolation_performance_test.R 2 0.005
~/singularity/slurm_nevrome_coest.sh short 64 100 R/crossvalidation/interpolation_performance_test.R 3 0.01
~/singularity/slurm_nevrome_coest.sh short 64 100 R/crossvalidation/interpolation_performance_test.R 4 0.05
~/singularity/slurm_nevrome_coest.sh short 64 100 R/crossvalidation/interpolation_performance_test.R 5 0.1

