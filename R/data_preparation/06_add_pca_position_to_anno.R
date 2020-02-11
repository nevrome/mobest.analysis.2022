load("data/anno_old.RData")
load("data/pca/pca.1240K_HumanOrigins.RData")

anno_old <- anno_old %>% dplyr::left_join(
  pca[, 1:3], 
  by = c("sample_id" = "Name")
)

save(anno_old, file = "data/anno_old_pca.RData")
