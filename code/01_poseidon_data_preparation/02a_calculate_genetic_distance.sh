cd data/poseidon_data/identical_filter

# pruning
plink1.9 --bfile ../poseidon_extracted/poseidon_extracted --exclude ../../../code/01_poseidon_data_preparation/myrange.txt --range --maf --make-bed --out poseidon_extracted.pruned

plink1.9 --bfile poseidon_extracted.pruned --distance triangle 1-ibs

