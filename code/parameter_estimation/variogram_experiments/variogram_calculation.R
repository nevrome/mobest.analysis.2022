library(magrittr)
library(ggplot2)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final

# geo distance
d_geo <- dist(anno %>% dplyr::select(x,y), "euclidean") %>% as.matrix()
rownames(d_geo) <- anno$sample_id
colnames(d_geo) <- anno$sample_id
d_geo_long <- d_geo %>% reshape2::melt(value.name = "geo_dist") %>%
  dplyr::mutate(
    # m to km
    geo_dist = geo_dist/1000
  )

# time distance
d_time <- dist(anno %>% dplyr::select(calage_center), "euclidean") %>% as.matrix()
rownames(d_time) <- anno$sample_id
colnames(d_time) <- anno$sample_id
d_time_long <- d_time %>% reshape2::melt(value.name = "time_dist")

# pca distance
d_pca <- dist(anno %>% dplyr::select(PC1, PC2), "euclidean") %>% as.matrix()
rownames(d_pca) <- anno$sample_id
colnames(d_pca) <- anno$sample_id
d_pca_long <- d_pca %>% reshape2::melt(value.name = "pca_dist")

# mds distance
d_mds <- dist(anno %>% dplyr::select(C1, C2), "euclidean") %>% as.matrix()
rownames(d_mds) <- anno$sample_id
colnames(d_mds) <- anno$sample_id
d_mds_long <- d_mds %>% reshape2::melt(value.name = "mds_dist")

# genetic single dimension residual distance
dat <- anno

model <- lm(PC1 ~ x + y + calage_center, data = dat)
dat <- dat %>% modelr::add_residuals(model, var = "PC1_resid")
model <- lm(PC2 ~ x + y + calage_center, data = dat)
dat <- dat %>% modelr::add_residuals(model, var = "PC2_resid")
model <- lm(C1 ~ x + y + calage_center, data = dat)
dat <- dat %>% modelr::add_residuals(model, var = "C1_resid")
model <- lm(C2 ~ x + y + calage_center, data = dat)
dat <- dat %>% modelr::add_residuals(model, var = "C2_resid")

d_gsd_long <- tidyr::expand_grid(d1 = dat, d2 = dat) %>%
  tidyr::unpack(cols = c(d1, d2), names_sep = "_") %>%
  dplyr::transmute(
    Var1 = d1_sample_id,
    Var2 = d2_sample_id,
    PC1_dist = abs(d1_PC1 - d2_PC1),
    PC1_dist_resid = abs(d1_PC1_resid - d2_PC1_resid),
    PC2_dist = abs(d1_PC2 - d2_PC2),
    PC2_dist_resid = abs(d1_PC2_resid - d2_PC2_resid),
    C1_dist = abs(d1_C1 - d2_C1),
    C1_dist_resid = abs(d1_C1_resid - d2_C1_resid),
    C2_dist = abs(d1_C2 - d2_C2),
    C2_dist_resid = abs(d1_C2_resid - d2_C2_resid)
  )

# merging
d_all <- d_geo_long %>%
  dplyr::full_join(
    d_time_long,by = c("Var1", "Var2")
  ) %>%
  dplyr::full_join(
    d_pca_long, by = c("Var1", "Var2")
  ) %>%
  dplyr::full_join(
    d_mds_long, by = c("Var1", "Var2")
  ) %>%
  dplyr::full_join(
    d_gsd_long, by = c("Var1", "Var2")
  ) %>%
  tibble::as_tibble()

save(d_all, file = "data/parameter_exploration/variogram/all_distances.RData")
