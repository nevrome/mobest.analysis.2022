#### load data ####
load("data/anno_1240K_and_anno_1240K_HumanOrigins.RData")
load("data/pca/pca.1240K_HumanOrigins.RData")

#### merge ####
anno_1240K_and_anno_1240K_HumanOrigins_pca <- anno_1240K_and_anno_1240K_HumanOrigins %>% dplyr::left_join(
  pca[, 1:3], 
  by = c("sample_id" = "Name")
)

#### save ####
save(anno_1240K_and_anno_1240K_HumanOrigins_pca, file = "data/anno_1240K_and_anno_1240K_HumanOrigins_pca.RData")
