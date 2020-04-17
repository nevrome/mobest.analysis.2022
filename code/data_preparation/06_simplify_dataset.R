#### load data ####
load("data/anno_1240K_and_anno_1240K_HumanOrigins_pca.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_pca

#### filter rows ####
anno <- anno %>% dplyr::filter(
  # temporal info available
  sapply(anno$age_prob_distribution_BC, nrow) > 0,
  # temporal: not modern
  age_string != "present", 
  # spatial info present
  !is.na(lat) & !is.na(lon)
)

#### select columns ####
anno <- anno %>% dplyr::select(
  sample_id, group_label,
  lat, lon,
  age_prob_distribution_BC, calage_center, calage_sample,
  PC1, PC2, PC3, PC4
)

#### store output dataset ####
anno_1240K_and_anno_1240K_HumanOrigins_simple <- anno
save(anno_1240K_and_anno_1240K_HumanOrigins_simple, file = "data/anno_1240K_and_anno_1240K_HumanOrigins_simple.RData")
