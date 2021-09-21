#!/bin/bash
#
#$ -S /bin/bash #defines bash as the shell for execution
#$ -N osearch #Name of the command that will be listed in the queue
#$ -cwd #change to current directory
#$ -j y #join error and standard output in one file, no error file will be written
#$ -o ~/log #standard output file or directory (joined with error because of -j y)
#$ -q archgen.q #queue
#$ -pe smp 4 #needs X CPU cores
#$ -l h_vmem=20G #request XGb of memory
#$ -V # load personal profile
#$ -t 1-100 # array job length
#$ -tc 10 # number of concurrently running tasks in array

date 

singularity exec --bind=/mnt/archgen/users/schmid ../singularity/images/nevrome_mobest/nevrome_mobest.sif Rscript code/06_alternative_parameter_exploration/different_rearview_distances/02a_origin_search_pipeline-age_resampling+one_kernel_setting.R ${SGE_TASK_ID}

date
 
exit 0
