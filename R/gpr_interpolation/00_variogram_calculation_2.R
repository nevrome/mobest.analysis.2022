library(magrittr)
library(ggplot2)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered

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
d_pca_long <- d_pca %>% reshape2::melt(value.name = "pca_dist") %>%
  dplyr::mutate(
    # distance to closeness
    pca_close = max(pca_dist) - pca_dist
  )

# merge distances
d_all <- d_geo_long %>%
  dplyr::full_join(
    d_time_long,by = c("Var1", "Var2")
  ) %>%
  dplyr::full_join(
    d_pca_long, by = c("Var1", "Var2")
  ) %>% tibble::as_tibble()

# binning of distances for simpler handling
d_cut <- d_all %>%
  dplyr::mutate(
    geo_dist_cut = cut(geo_dist, breaks = seq(0, max(geo_dist), 100), labels = F) * 100,
    time_dist_cut = cut(time_dist, breaks = seq(0, max(time_dist), 100), labels = F) * 100
  ) %>%
  dplyr::group_by(geo_dist_cut, time_dist_cut) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_pca_dist = mean(pca_dist),
    mean_pca_close = mean(pca_close)
  ) %>%
  dplyr::ungroup()

# line plot distance
d_cut %>% ggplot() +
  geom_path(
    aes(x = geo_dist_cut, y = mean_pca_dist, group = time_dist_cut, color = time_dist_cut)
  ) +
  viridis::scale_color_viridis()

# map plot
d_cut %>% ggplot() +
  geom_raster(
    aes(x = geo_dist_cut, y = time_dist_cut, fill = mean_pca_dist)
  ) +
  viridis::scale_fill_viridis()

d_cut %>% ggplot() +
  geom_raster(
    aes(x = geo_dist_cut, y = time_dist_cut, fill = n)
  ) +
  viridis::scale_fill_viridis()

# line plot closeness
d_cut %>% ggplot() +
  geom_path(
    aes(x = geo_dist_cut, y = mean_pca_close, group = time_dist_cut, color = time_dist_cut)
  ) +
  viridis::scale_color_viridis()

# fit model test for space
d_cut_xy <- d_cut %>%
  dplyr::mutate(
    x = geo_dist_cut,
    y = mean_pca_close
  ) %>%
  dplyr::filter(time_dist_cut == 100) 

start <- c(k = 0.2, d = 100000)
fit = nls(y ~ I(k * exp((-(x^2) / d))), data = d_cut_xy, start = start)

d_cut_xy %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  stat_smooth(method = nls, formula = as.formula(fit), method.args = list(start = start), se = FALSE, color = "black")

## 2D
d_cut_xyz <- d_cut %>%
  dplyr::transmute(
    x = geo_dist_cut,
    y = time_dist_cut,
    z = mean_pca_close
  )

start <- c(k = 0.2, dspace = 100000, dtime = 100000)
fit = nls(z ~ I((k * exp((-(x^2) / dspace))) * (k * exp((-(y^2) / dtime)))), data = d_cut_xyz, start = start)

pred_grid <- expand.grid(
  x = seq(100, max(d_cut_xyz$x, na.rm = T), 100),
  y = seq(100, max(d_cut_xyz$y, na.rm = T), 100)
)
pred_grid$z <- predict(
  fit,
  pred_grid
) %>% as.numeric()

ggplot() +
  geom_raster(
    data = d_cut_xyz,
    aes(x, y, fill = z)
  ) +
  viridis::scale_fill_viridis()

ggplot() +
  geom_raster(
    data = pred_grid,
    aes(x, y, fill = z),
  ) +
  viridis::scale_fill_viridis()

# distance between prediction and measurements
pred_test <- pred_grid %>%
  dplyr::left_join(d_cut_xyz, by = c("x", "y"), suffix = c(".pred", ".meas")) %>%
  dplyr::mutate(
    dev = abs(z.pred - z.meas) 
  )

ggplot() +
  geom_raster(
    data = pred_test,
    aes(x, y, fill = dev),
  ) +
  viridis::scale_fill_viridis(option = "plasma")

#### limit analysis to 1000km 1000years analysis ####
d_cut_1000 <- d_all %>%
  dplyr::filter(
    geo_dist < 1000, time_dist < 1000
  ) %>% dplyr::mutate(
    geo_dist_cut = cut(geo_dist, breaks = seq(0, max(geo_dist), 100), labels = F) * 100,
    time_dist_cut = cut(time_dist, breaks = seq(0, max(time_dist), 100), labels = F) * 100
  ) %>%
  dplyr::group_by(geo_dist_cut, time_dist_cut) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_pca_dist = mean(pca_dist),
    mean_pca_close = mean(pca_close)
  ) %>%
  dplyr::ungroup()

# line plot distance
d_cut_1000 %>% ggplot() +
  geom_path(
    aes(x = geo_dist_cut, y = mean_pca_dist, group = time_dist_cut, color = time_dist_cut)
  ) +
  viridis::scale_color_viridis()

# map plot
d_cut_1000 %>% ggplot() +
  geom_raster(
    aes(x = geo_dist_cut, y = time_dist_cut, fill = mean_pca_dist)
  ) +
  viridis::scale_fill_viridis()

d_cut_1000_xyz <- d_cut_1000 %>%
  dplyr::transmute(
    x = geo_dist_cut,
    y = time_dist_cut,
    z = mean_pca_close
  )

start_1000 <- c(k = 0.2, dspace = 100000, dtime = 100000)
fit_1000 = nls(z ~ I((k * exp((-(x^2) / dspace))) * (k * exp((-(y^2) / dtime)))), data = d_cut_1000_xyz, start = start_1000)

pred_grid_1000 <- expand.grid(
  x = seq(100, max(d_cut_1000_xyz$x, na.rm = T), 100),
  y = seq(100, max(d_cut_1000_xyz$y, na.rm = T), 100)
)
pred_grid_1000$z <- predict(
  fit_1000,
  pred_grid_1000
) %>% as.numeric()

ggplot() +
  geom_raster(
    data = pred_grid_1000,
    aes(x, y, fill = z),
  ) +
  viridis::scale_fill_viridis()
