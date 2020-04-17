load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")

anno_1240K_and_anno_1240K_HumanOrigins_filtered$group_label %>% 
  unique() %>%
  writeLines("code/mds/old_pops_1240K_HumanOrigins.txt")
