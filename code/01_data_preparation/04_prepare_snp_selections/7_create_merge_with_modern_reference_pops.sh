cd data/genotype_data

trident forge \
  --forgeFile ../../code/01_data_preparation/modern_western_eurasian_populations.txt \
  -d aadrv50_1240K_HO \
  -o aadrv50_1240K_HO_Western_Eurasia_modern
  --minimal


#### TODO ####

trident forge \
  -d aadrv50_1240K_HO_Western_Eurasia_modern \
  -d snp_subsets/... \
  -f "**" \
  -o unfiltered_snp_selection_with_modern_reference_pops
  
trident forge \
  --forgeFile ../../../code/01_data_preparation/modern_western_eurasian_populations.txt \
  -d filtered_snp_selection \
  -o filtered_snp_selection_with_modern_reference_pops
  
