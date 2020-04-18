#scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/mds/1240K_HumanOrigins.pruned.mds .
#scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/mds/1240K_HumanOrigins.filtered.pruned.mds .

library(magrittr)
library(ggplot2)

mds1 <- readr::read_delim("data/mds/1240K_HumanOrigins.pruned.mds", " ", trim_ws = T) %>%
  dplyr::select(-X8)

mds2 <- readr::read_delim("data/mds/1240K_HumanOrigins.filtered.pruned.mds", " ", trim_ws = T) %>%
  dplyr::select(-X8)


load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered

mds_worked <- mds2 %>% dplyr::left_join(
  anno, by = c("IID" = "sample_id")
)

mds_worked %>% ggplot() +
  geom_point(aes(C1, C2))

mds_worked %>% ggplot() +
  geom_point(aes(C1, PC1))

mds_worked %>% ggplot() +
  geom_point(aes(C2, PC2))
