library(magrittr)

load("data/poseidon_data/janno_final.RData")
load("data/parameter_exploration/variogram/all_distances.RData")

d_all_long <- d_all %>% tidyr::pivot_longer(
  cols = c(C1_dist_resid, C2_dist_resid, C3_dist_resid),
  names_to = "dist_type", values_to = "dist_val"
)

lower_left_variogram <- d_all_long %>%
  dplyr::filter(time_dist < 50 & geo_dist < 50) %>%
  dplyr::filter(Var1 != Var2) %>%
  dplyr::mutate(
    dist_type = replace(dist_type, dist_type == "C1_dist_resid", "C1"),
    dist_type = replace(dist_type, dist_type == "C2_dist_resid", "C2"),
    dist_type = replace(dist_type, dist_type == "C3_dist_resid", "C3"),
    dist_type = factor(dist_type, levels = c("C1", "C2", "C3")),
    # rescaling of the dist val to a relative proportion
    dist_val_adjusted = dplyr::case_when(
      dist_type == "C1" ~ 0.5*(dist_val^2/var(janno_final$C1)),
      dist_type == "C2" ~ 0.5*(dist_val^2/var(janno_final$C2)),
      dist_type == "C3" ~ 0.5*(dist_val^2/var(janno_final$C3)),
    )
  )

estimated_nuggets <- lower_left_variogram %>%
  dplyr::group_by(dist_type) %>%
  dplyr::summarise(mean = mean(dist_val_adjusted, na.rm = T))

nuggets_to_use <- as.character(c(
  estimated_nuggets$mean[1:2] %>% max %>% round(., 2),
  estimated_nuggets$mean[1:3] %>% max %>% round(., 2)
))

save(lower_left_variogram, file = "data/parameter_exploration/variogram/lower_left_variogram.RData")
save(estimated_nuggets, file = "data/parameter_exploration/variogram/estimated_nuggets.RData")
writeLines(nuggets_to_use, "data/parameter_exploration/variogram/nuggets.txt")
