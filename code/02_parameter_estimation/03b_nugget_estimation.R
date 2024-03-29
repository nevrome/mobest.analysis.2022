library(magrittr)

load("data/genotype_data/janno_final.RData")
load("data/parameter_exploration/targeted/all_distances.RData")

d_all_long <- d_all %>% tidyr::pivot_longer(
  cols = c(C1_mds_u_dist_resid, C2_mds_u_dist_resid, C3_mds_u_dist_resid),
  names_to = "dist_type", values_to = "dist_val"
) %>%
  dplyr::mutate(
    dist_type = dplyr::recode(
      dist_type,
      C1_mds_u_dist_resid = "C1_mds_u",
      C2_mds_u_dist_resid = "C2_mds_u",
      C3_mds_u_dist_resid = "C3_mds_u"
    )
  )

lower_left_variogram <- d_all_long %>%
  # filter for small temporal and spatial pairwise distances
  dplyr::filter(time_dist < 50 & geo_dist < 50) %>%
  dplyr::filter(id1 != id2) %>%
  dplyr::mutate(
    # rescaling of the dist val to a relative proportion
    dist_val_adjusted = dplyr::case_when(
      dist_type == "C1_mds_u" ~ 0.5*(dist_val^2/stats::var(janno_final$C1_mds_u)),
      dist_type == "C2_mds_u" ~ 0.5*(dist_val^2/stats::var(janno_final$C2_mds_u)),
      dist_type == "C3_mds_u" ~ 0.5*(dist_val^2/stats::var(janno_final$C3_mds_u)),
    )
  )

estimated_nuggets <- lower_left_variogram %>%
  dplyr::group_by(dist_type) %>%
  dplyr::summarise(nugget = mean(dist_val_adjusted, na.rm = T)) %>%
  dplyr::mutate(
    dependent_var_id = gsub("_dist", "", dist_type)
  )

save(lower_left_variogram, file = "data/parameter_exploration/targeted/lower_left_variogram.RData")
save(estimated_nuggets, file = "data/parameter_exploration/targeted/estimated_nuggets.RData")
