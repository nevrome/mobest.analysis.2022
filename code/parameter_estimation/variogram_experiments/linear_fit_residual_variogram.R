library(magrittr)
library(ggplot2)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
rawdat <- anno_1240K_and_anno_1240K_HumanOrigins_final
dat <- rawdat %>%
  dplyr::rename(t = calage_center) %>%
  dplyr::select(sample_id, x, y, t, PC1)

model <- lm(PC1 ~ x + y + t, data = dat)

model

dat_w_resid <- dat %>% modelr::add_residuals(model, var = "PC1_resid")

# distance calc
dist_table <- tidyr::expand_grid(d1 = dat_w_resid, d2 = dat_w_resid) %>%
  tidyr::unpack(cols = c(d1, d2), names_sep = "_") %>%
  dplyr::transmute(
    sample_pair = paste(d1_sample_id, d2_sample_id, sep = "_"),
    geo_dist = sqrt((d1_x - d2_x)^2 + (d1_y - d2_y)^2) / 1000,
    time_dist = abs(d1_t - d2_t),
    PC1_dist = abs(d1_PC1 - d2_PC1),
    PC1resid_dist = abs(d1_PC1_resid - d2_PC1_resid)
  )

# binning
dist_table_binned <- dist_table %>%
  dplyr::mutate(
    geo_dist_cut = cut(geo_dist, breaks = 50, labels = F),
    time_dist_cut = cut(time_dist, breaks = 10, labels = F)
  ) %>%
  dplyr::group_by(geo_dist_cut, time_dist_cut) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_sq_pca_dist = mean(PC1_dist^2),
    mean_sq_pca_resid_dist = mean(PC1resid_dist^2)
  ) %>%
  dplyr::ungroup()

dist_table_binned %>%
  tidyr::pivot_longer(cols = c(mean_sq_pca_dist, mean_sq_pca_resid_dist),
                      names_to = "PC1_type", values_to = "PC1_val") %>%
  ggplot() +
  geom_line(mapping = aes(x = geo_dist_cut, y = PC1_val, group = time_dist_cut, col = time_dist_cut)) +
  facet_wrap(~PC1_type)

dist_table %>%
  dplyr::filter(time_dist < 20 & geo_dist < 2000) %>%
  ggplot(mapping = aes(x = geo_dist, y = PC1resid_dist)) + geom_point(alpha=0.01) +
  geom_smooth()

# dist_table %>%
#   dplyr::filter(time_dist < 50 & geo_dist < 50) %>%
#   dplyr::filter(geo_dist != 0 & time_dist != 0) %>%
#   ggplot() +
#   # geom_histogram(
#   #   aes(PC1_dist), bins = 1000
#   # )
#   geom_boxplot(
#     aes(PC1_dist)
#   )
# 
# dist_table %>%
#   dplyr::filter(time_dist < 50 & geo_dist < 50) %>%
#   dplyr::filter(geo_dist != 0 & time_dist != 0) %$%
#   mean(PC1_dist)

dist_table %>%
  dplyr::filter(time_dist < 4000 & geo_dist < 50) %>%
  ggplot(mapping = aes(x = time_dist, y = PC1resid_dist)) + geom_point(alpha=0.01) +
  geom_smooth()
