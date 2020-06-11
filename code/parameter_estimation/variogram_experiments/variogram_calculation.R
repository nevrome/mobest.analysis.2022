library(magrittr)
library(ggplot2)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final

# calculate distances
d_geo <- dist(anno %>% dplyr::select(x,y), "euclidean") %>% as.matrix()
rownames(d_geo) <- anno$sample_id
colnames(d_geo) <- anno$sample_id
d_geo_long <- d_geo %>% reshape2::melt(value.name = "geo_dist") %>%
  dplyr::mutate(
    # m to km
    geo_dist = geo_dist/1000
  )

d_time <- dist(anno %>% dplyr::select(calage_center), "euclidean") %>% as.matrix()
rownames(d_time) <- anno$sample_id
colnames(d_time) <- anno$sample_id
d_time_long <- d_time %>% reshape2::melt(value.name = "time_dist")

d_pca <- dist(anno %>% dplyr::select(PC1, PC2), "euclidean") %>% as.matrix()
rownames(d_pca) <- anno$sample_id
colnames(d_pca) <- anno$sample_id
d_pca_long <- d_pca %>% reshape2::melt(value.name = "pca_dist")

# Merging
d_all <- d_geo_long %>%
  dplyr::full_join(
    d_time_long,by = c("Var1", "Var2")
  ) %>%
  dplyr::full_join(
    d_pca_long, by = c("Var1", "Var2")
  ) %>% tibble::as_tibble()

# Binning
d_binned <- d_all %>%
  dplyr::mutate(
    geo_dist_cut = cut(geo_dist, breaks = c(seq(0, max(geo_dist), 100), max(geo_dist)), include.lowest	= T, labels = F) * 100,
    time_dist_cut = cut(time_dist, breaks = c(seq(0, max(time_dist), 500), max(time_dist)), include.lowest	= T, labels = F) * 500
  ) %>%
  dplyr::group_by(geo_dist_cut, time_dist_cut) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_sq_pca_dist = mean(pca_dist^2)
  ) %>%
  dplyr::ungroup()

save(d_binned, file="data/parameter_exploration/variogram/binned_data.RData")
