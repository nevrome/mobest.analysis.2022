#### load merged datasets ####
load("data/anno_1240K_and_anno_1240K_HumanOrigins.RData")

anno <- anno_1240K_and_anno_1240K_HumanOrigins

#### prepare anno_old ####

anno_old <- anno %>% dplyr::filter(
  # temporal info available
  sapply(anno$age_prob_distribution_BC, nrow) > 0,
  # temporal: not modern
  age_string != "present", 
  # spatial info present
  !is.na(lat) & !is.na(lon)
)

#### add important id ####
anno_old$old_id <- 1:nrow(anno_old)

#### store output dataset ####
save(anno_old, file = "data/anno_old.RData")
