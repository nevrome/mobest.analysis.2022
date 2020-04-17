#scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/mds/plink.mds .

library(magrittr)
library(ggplot2)

mds <- readr::read_delim("data/mds/plink.mds", " ", trim_ws = T) %>%
  dplyr::select(-X8)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered

anno  <- anno %>% dplyr::filter(
  sample_id != "I4933"
)

mds_worked <- anno %>% dplyr::left_join(
  mds, by = c("sample_id" = "IID")
) %>% dplyr::filter(
  C2 < 0.04,
  C2 > -0.03
)

mds_worked %>% ggplot() +
  geom_point(aes(C2, PC4))


mds_worked <- mds %>% dplyr::filter(
  IID != "I4933"
) %>% dplyr::left_join(
  anno, by = c("IID" = "sample_id")
) %>%
  dplyr::mutate(
    SG = grepl(".SG", IID)
  ) 




