#### load datasets ####
load("data/anno_1240K.RData")
load("data/anno_1240K_HumanOrigins.RData")

#### merge datasets ####
anno_1240K_and_anno_1240K_HumanOrigins_raw <- dplyr::left_join(
  anno_1240K_HumanOrigins %>% dplyr::select(sample_id, group_label, site, lat, lon),
  anno_1240K %>% dplyr::select(instance_id, age_string),
  by = c("sample_id" = "instance_id")
)

#### store merged dataset ####
save(anno_1240K_and_anno_1240K_HumanOrigins_raw, file = "data/anno_1240K_and_anno_1240K_HumanOrigins_raw.RData")
