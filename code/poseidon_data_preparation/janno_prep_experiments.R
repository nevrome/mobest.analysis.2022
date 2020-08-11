library(magrittr)

janno <- poseidon2::read_janno("data/poseidon_data/poseidon_merged_dataset/poseidon2_merged.janno")

janno %>% poseidon2::process_age()

# QC filter
# Nr_autosomal_SNPs? Coverage_1240K? Endogenous?, Damage?, Xcontam?, mtContam?
