#!/bin/bash
qsub <<EOT
#!/bin/bash

# run with eg. ./grid_nevrome_coest.sh 1 4 test.R
# $1 Number of cores
# $2 GB Ram
# $3 Rscript to run
# $4-X pass to Rscript

#$ -S /bin/bash #defines bash as the shell for execution
#$ -N mobest #Name of the command that will be listed in the queue
#$ -cwd #change to current directory
#$ -j y #join error and standard output in one file, no error file will be written
#$ -o ~/log #standard output file or directory (joined with error because of -j y)
#$ -q archgen.q #queue
#$ -pe smp "$1" #needs 8 CPU cores
#$ -l h_vmem="$2"G #request 4Gb of memory
#$ -V # load personal profile

date
pwd
singularity exec --bind=/mnt/archgen/users/schmid singularity_mobest.sif "$3" "${@:4}"
date

exit 0
EOT
