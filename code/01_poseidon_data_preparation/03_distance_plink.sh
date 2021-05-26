#!/bin/bash

#$ -S /bin/bash #defines bash as the shell for execution
#$ -N mobest #Name of the command that will be listed in the queue
#$ -cwd #change to current directory
#$ -j y #join error and standard output in one file, no error file will be written
#$ -o mobest_qsub_output #standard output file or directory (joined with error because of -j y)
#$ -q archgen.q #queue
#$ -pe make 10 #needs 8 CPU cores
#$ -l h_vmem=40G #request 4Gb of memory
#$ -V # load personal profile

date

cd data/poseidon_data/identical_filter

# pruning
plink --bfile ../poseidon_extracted_pre_identicals_filter/poseidon_extracted_pre_identicals_filter --exclude ../../../code/01_poseidon_data_preparation/myrange.txt --range --maf --make-bed --out poseidon_extracted_pre_identicals_filter.pruned

# pairwise distances
plink --bfile poseidon_extracted_pre_identicals_filter.pruned --distance square 1-ibs

date

exit 0