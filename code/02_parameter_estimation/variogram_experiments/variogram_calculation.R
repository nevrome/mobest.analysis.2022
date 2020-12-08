library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")

mobest::calculate_all_distances(
  mobest::create_spatpos(
    id = janno_final$Individual_ID,
    x = janno_final$x,
    y = janno_final$y,
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  mobest::create_obs(
    id = janno_final$Individual_ID,
    C1 = janno_final$C1,
    C2 = janno_final$C2
  )
)

# geo distance
d_geo <- dist(janno_final %>% dplyr::select(x,y), "euclidean") %>% as.matrix()
rownames(d_geo) <- janno_final$Individual_ID
colnames(d_geo) <- janno_final$Individual_ID
d_geo_long <- d_geo %>% reshape2::melt(value.name = "geo_dist") %>%
  dplyr::mutate(
    # m to km
    geo_dist = geo_dist/1000
  )

# time distance
d_time <- dist(janno_final %>% dplyr::select(Date_BC_AD_Median_Derived), "euclidean") %>% as.matrix()
rownames(d_time) <- janno_final$Individual_ID
colnames(d_time) <- janno_final$Individual_ID
d_time_long <- d_time %>% reshape2::melt(value.name = "time_dist")

# mds distance
d_mds <- dist(janno_final %>% dplyr::select(C1, C2), "euclidean") %>% as.matrix()
rownames(d_mds) <- janno_final$Individual_ID
colnames(d_mds) <- janno_final$Individual_ID
d_mds_long <- d_mds %>% reshape2::melt(value.name = "mds_dist")

# genetic single dimension residual distance
dat <- janno_final

model <- lm(C1 ~ x + y + Date_BC_AD_Median_Derived, data = dat)
dat <- dat %>% modelr::add_residuals(model, var = "C1_resid")
model <- lm(C2 ~ x + y + Date_BC_AD_Median_Derived, data = dat)
dat <- dat %>% modelr::add_residuals(model, var = "C2_resid")

d_gsd_long <- tidyr::expand_grid(d1 = dat, d2 = dat) %>%
  tidyr::unpack(cols = c(d1, d2), names_sep = "_") %>%
  dplyr::transmute(
    Var1 = d1_Individual_ID,
    Var2 = d2_Individual_ID,
    C1_dist = abs(d1_C1 - d2_C1),
    C1_dist_resid = abs(d1_C1_resid - d2_C1_resid),
    C2_dist = abs(d1_C2 - d2_C2),
    C2_dist_resid = abs(d1_C2_resid - d2_C2_resid),
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
    d_mds_long, by = c("Var1", "Var2")
  ) %>%
  dplyr::full_join(
    d_gsd_long, by = c("Var1", "Var2")
  ) %>%
  tibble::as_tibble()

save(d_all, file = "data/parameter_exploration/variogram/all_distances.RData")

