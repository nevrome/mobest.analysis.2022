cd data/genotype_data/snp_subsets

trident forge \
  --forgeFile ../../../code/01_data_preparation/ind_list_post_snp_selection.txt \
  -d unfiltered_snp_selection_pre_ind_correction \
  -o unfiltered_snp_selection
  
trident genoconvert \
  -d unfiltered_snp_selection \
  --outFormat EIGENSTRAT
  
trident forge \
  --forgeFile ../../../code/01_data_preparation/ind_list_post_snp_selection.txt \
  -d filtered_snp_selection_pre_ind_correction \
  -o filtered_snp_selection

trident genoconvert \
  -d filtered_snp_selection \
  --outFormat EIGENSTRAT