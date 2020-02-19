library(magrittr)
library(ggplot2)

# calculate distances


load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered

d_geo <- dist(anno %>% dplyr::select(x,y), "euclidean") %>% as.matrix()
rownames(d_geo) <- anno$sample_id
colnames(d_geo) <- anno$sample_id
d_geo_long <- d_geo %>% reshape2::melt(value.name = "geo_dist")

d_time <- dist(anno %>% dplyr::select(calage_center), "euclidean") %>% as.matrix()
rownames(d_time) <- anno$sample_id
colnames(d_time) <- anno$sample_id
d_time_long <- d_time %>% reshape2::melt(value.name = "time_dist")

d_pca <- dist(anno %>% dplyr::select(PC1, PC2), "euclidean") %>% as.matrix()
rownames(d_pca) <- anno$sample_id
colnames(d_pca) <- anno$sample_id
d_pca_long <- d_pca %>% reshape2::melt(value.name = "pca_dist")

d_all <- d_geo_long %>%
  dplyr::full_join(
    d_time_long,by = c("Var1", "Var2")
  ) %>%
  dplyr::full_join(
    d_pca_long, by = c("Var1", "Var2")
  ) %>% tibble::as_tibble()

# bin
d_cut <- d_all %>%
  dplyr::mutate(
    geo_dist_cut = cut(geo_dist, breaks = seq(0, max(geo_dist), 100000), labels = F),
    time_dist_cut = cut(time_dist, breaks = seq(0, max(time_dist), 100), labels = F)
  ) %>%
  dplyr::group_by(geo_dist_cut, time_dist_cut) %>%
  dplyr::summarise(mean_pca_dist = mean(pca_dist)) %>%
  dplyr::ungroup()

d_cut %>% ggplot() +
  geom_raster(
    aes(x = geo_dist_cut, y = time_dist_cut, fill = mean_pca_dist)
  )



