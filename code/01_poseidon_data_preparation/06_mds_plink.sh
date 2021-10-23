#!/bin/bash

#$ -S /bin/bash #defines bash as the shell for execution
#$ -N dist #Name of the command that will be listed in the queue
#$ -cwd #change to current directory
#$ -j y #join error and standard output in one file, no error file will be written
#$ -o ~/log #standard output file or directory (joined with error because of -j y)
#$ -q archgen.q #queue
#$ -pe smp 10 #needs 8 CPU cores
#$ -l h_vmem=40G #request 4Gb of memory
#$ -V # load personal profile

date

cd data/poseidon_data/mds

# pruning
plink1.9 --bfile ../poseidon_extracted/poseidon_extracted --exclude ../../../code/01_poseidon_data_preparation/myrange.txt --range --maf --make-bed --out poseidon_extracted.pruned

# generate general pairwise stats
plink1.9 --bfile poseidon_extracted.pruned --genome --out poseidon_extracted.pruned

# calculate mds 
# (in the plink mds implementation there is no difference in the result for C1 and 
# C2 with --mds-plot 2 or --mds-plot 3. So it's sufficient to call --mds-plot 3 once
# and then select the desired dimensions later)
plink1.9 --bfile poseidon_extracted.pruned --cluster --mds-plot 3 --read-genome poseidon_extracted.pruned.genome --out mds

date

exit 0

