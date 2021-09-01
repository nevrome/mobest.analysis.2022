#!/bin/bash

#$ -S /bin/bash #defines bash as the shell for execution
#$ -N mobest #Name of the command that will be listed in the queue
#$ -cwd #change to current directory
#$ -j y #join error and standard output in one file, no error file will be written
#$ -o mobest_qsub_output #standard output file or directory (joined with error because of -j y)
#$ -q archgen.q #queue
#$ -pe smp 10 #needs 8 CPU cores
#$ -l h_vmem=40G #request 4Gb of memory
#$ -V # load personal profile

date

cd data/poseidon_data/mds

# pruning
plink --bfile ../poseidon_extracted/poseidon_extracted --exclude ../../../code/01_poseidon_data_preparation/myrange.txt --range --maf --make-bed --out poseidon_extracted.pruned

# generate general pairwise stats
plink --bfile poseidon_extracted.pruned --genome --out poseidon_extracted.pruned

# create mds table
plink --bfile poseidon_extracted.pruned --cluster --mds-plot 3 --read-genome poseidon_extracted.pruned.genome --out poseidon_extracted.pruned

date

exit 0

