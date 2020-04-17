library(magrittr)
library(ggplot2)

# calculate distances


load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered

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
    # m to km
    pca_close = max(pca_dist) - pca_dist
  )

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
    geo_dist_cut = cut(geo_dist, breaks = seq(0, max(geo_dist), 100), labels = F),
    time_dist_cut = cut(time_dist, breaks = seq(0, max(time_dist), 100), labels = F)
  ) %>%
  dplyr::group_by(geo_dist_cut, time_dist_cut) %>%
  dplyr::summarise(
    mean_pca_dist = mean(pca_dist),
    mean_pca_close = mean(pca_close)
  ) %>%
  dplyr::ungroup()

# line plot
d_cut %>% ggplot() +
  geom_path(
    aes(x = geo_dist_cut, y = mean_pca_close, group = time_dist_cut, color = time_dist_cut)
  ) +
  viridis::scale_color_viridis()

# map plot
d_cut %>% ggplot() +
  geom_raster(
    aes(x = geo_dist_cut, y = time_dist_cut, fill = mean_pca_close)
  ) +
  viridis::scale_fill_viridis()

# fit model
d_cut_xy <- d_cut %>%
  dplyr::mutate(
    x = as.double(geo_dist_cut),
    y = mean_pca_close
  ) %>%
  dplyr::filter(time_dist_cut == 20) 

d_cut_xy %>% ggplot() +
  geom_path(aes(x, y))

kernel_theta <- function(distance, d) { 0.16 * exp(-(sum(distance^2)) / d) }

kernel_theta_data <- expand.grid(
  dist_p1_p2 = seq(0, 3, 0.01),
  d = c(0.01, 0.1, 1, 10, 100)
)

kernel_theta_data$k <- sapply(
  1:nrow(kernel_theta_data), function(i, kernel_theta_data) {
    kernel_theta(kernel_theta_data$dist_p1_p2[i], kernel_theta_data$d[i])
  },
  kernel_theta_data
)


fit = nls(y ~ I(exp((-(x^2) / d))), data = d_cut_xy, start = c(d = 1000))

d_cut_xy %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_line(
    data = kernel_theta_data,
    aes(dist_p1_p2, k, group = d),
    size = 1
  ) +
  stat_smooth(method = nls, formula = as.formula(fit), method.args = list(start = c(d = 1000)), se = FALSE, color = "black")


# 
# f <- function(par) {
#   m <- par[1]
#   d <- par[2]
#   rhat <- exp((-(x^2)) / d)
#   sum((r - rhat)^2)
# }
# 
# optim(c(10, 10), f, method = "BFGS", control = list(reltol=1e-9))


