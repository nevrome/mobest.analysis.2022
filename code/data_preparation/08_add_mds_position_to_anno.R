library(magrittr)

#### load data ####
load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
mds <- readr::read_delim("data/mds/1240K_HumanOrigins.filtered.pruned.mds", " ", trim_ws = T) %>%
  dplyr::select(-FID, -SOL, -X8)

#### merge ####
anno_1240K_and_anno_1240K_HumanOrigins_mds <- anno_1240K_and_anno_1240K_HumanOrigins_filtered %>% dplyr::left_join(
  mds, 
  by = c("sample_id" = "IID")
)

#### save ####
save(anno_1240K_and_anno_1240K_HumanOrigins_mds, file = "data/anno_1240K_and_anno_1240K_HumanOrigins_mds.RData")
