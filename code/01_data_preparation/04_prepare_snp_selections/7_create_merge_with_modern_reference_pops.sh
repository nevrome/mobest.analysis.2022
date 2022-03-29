cd data/genotype_data

# prepare modern pops poseidon package

trident forge \
  --forgeFile ../../code/01_data_preparation/modern_western_eurasian_populations.txt \
  -d aadrv50_1240K_HO \
  -o aadrv50_1240K_HO_Western_Eurasia_modern
  --minimal

# merge snp selection packages with modern pops

trident forge \
  -d aadrv50_1240K_HO_Western_Eurasia_modern \
  -d snp_subsets/unfiltered_snp_selection \
  -f "*aadrv50_1240K_HO_Western_Eurasia_modern*,*unfiltered_snp_selection*" \
  --intersect \
  -o snp_subsets/unfiltered_snp_selection_with_modern_reference_pops \
  --minimal \
  --no-extract \
  --outFormat EIGENSTRAT
  
trident forge \
  -d aadrv50_1240K_HO_Western_Eurasia_modern \
  -d snp_subsets/filtered_snp_selection \
  -f "*aadrv50_1240K_HO_Western_Eurasia_modern*,*filtered_snp_selection*" \
  --intersect \
  -o snp_subsets/filtered_snp_selection_with_modern_reference_pops \
  --minimal \
  --no-extract \
  --outFormat EIGENSTRAT
  