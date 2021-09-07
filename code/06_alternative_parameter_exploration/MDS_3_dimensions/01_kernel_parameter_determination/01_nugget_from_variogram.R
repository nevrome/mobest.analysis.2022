library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")

d_all <- mobest::calculate_pairwise_distances(
  independent = mobest::create_spatpos(
    id = janno_final$Individual_ID,
    x = janno_final$x,
    y = janno_final$y,
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = mobest::create_obs(
    C1 = janno_final$mds3_C1,
    C2 = janno_final$mds3_C2,
    C3 = janno_final$mds3_C3
  )
)

###

geo_scaled <- d_all$geo_dist/max(d_all$geo_dist)
time_scaled <- d_all$time_dist/max(d_all$time_dist)

d <- function(a, b, c = 0) {
  sqrt(a^2 + b^2 + c^2)
}
  
gdist3 <- Map(d, d_all$C1_dist, d_all$C2_dist, d_all$C3_dist) %>% unlist()
gdist2 <- Map(d, d_all$C1_dist, d_all$C2_dist) %>% unlist()
stdist <- Map(d, geo_scaled, time_scaled) %>% unlist()

distances <- tibble::tibble(
  stdist = stdist,
  gdist2 = gdist2,
  gdist3 = gdist3
) %>%
  tidyr::pivot_longer(cols = c("gdist2", "gdist3"), names_to = "genetic_distance_method", values_to = "gdist")

r2s <- tibble::tibble(
  genetic_distance_method = c("gdist2", "gdist3"),
  r2 = c(cor(spdist, gdist2)^2, cor(spdist, gdist3)^2)
)

ggplot() +
  geom_hex(
    data = distances,
    mapping = aes(x = stdist, y = gdist, col=..count..)
  ) +
  geom_text(
    data = r2s,
    mapping = aes(x = Inf, y = -Inf, label = paste("R2=", round(r2, 3))),
    hjust = "inward", vjust = "inward"
  ) +
  facet_wrap(~genetic_distance_method) +
  scale_color_gradient(low = "white", high = "black") +
  scale_fill_gradient(low = "white", high = "black")
  
  
  

###

d_binned <- mobest::bin_pairwise_distances(d_all)

d_all_long <- d_all %>% tidyr::pivot_longer(
  cols = c(C1_dist_resid, C2_dist_resid, C3_dist_resid),
  names_to = "dist_type", values_to = "dist_val"
)

lower_left <- d_all_long %>%
  dplyr::filter(time_dist < 50 & geo_dist < 50) %>%
  dplyr::filter(Var1 != Var2) %>%
  dplyr::mutate(
    dist_type = replace(dist_type, dist_type == "C1_dist_resid", "C1"),
    dist_type = replace(dist_type, dist_type == "C2_dist_resid", "C2"),
    dist_type = replace(dist_type, dist_type == "C3_dist_resid", "C3"),
    dist_type = factor(dist_type, levels = c("C1", "C2", "C3")),
    # rescaling of the dist val to a relative proportion
    dist_val_adjusted = dplyr::case_when(
      dist_type == "C1" ~ 0.5*(dist_val^2/var(janno_final$mds3_C1)),
      dist_type == "C2" ~ 0.5*(dist_val^2/var(janno_final$mds3_C2)),
      dist_type == "C3" ~ 0.5*(dist_val^2/var(janno_final$mds3_C3))
    )
  )

lower_left_mean <- lower_left %>%
  dplyr::group_by(
    dist_type
  ) %>%
  dplyr::summarise(
    mean = mean(dist_val_adjusted, na.rm = T)
  )

p <- ggplot() +
  geom_jitter(
    data = lower_left,
    mapping = aes(x = dist_type, y = dist_val_adjusted, color = dist_type),
    alpha = 0.5,
    size = 0.5,
    width = 0.4
  ) + 
  geom_point(
    data = lower_left_mean,
    mapping = aes(x = dist_type, y = mean),
    size = 2
  ) +
  geom_point(
    data = lower_left_mean,
    mapping = aes(x = dist_type, y = mean),
    size = 5, shape = "|"
  ) +
  geom_text(
    data = lower_left_mean,
    mapping = aes(x = dist_type, y = mean, label = paste0("mean: ~", round(mean, 3))),
    nudge_x = -0.5
  ) +
  coord_flip() +
  theme_bw() +
  guides(
    color = F
  ) +
  xlab("ancestry component distance type") +
  ylab("log10 pairwise half mean squared normalized residual distance") +
  scale_y_log10(labels = scales::comma) +
  scale_x_discrete(limits = rev(levels(lower_left$dist_type)))

ggsave(
  "plots/figure_sup_4_semivariogram_nugget.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 120, units = "mm",
  limitsize = F
)

###

d_binned_long <- d_binned %>%
  tidyr::pivot_longer(
    cols = tidyselect::one_of(c("C1_dist", "C2_dist", "C3_dist", "C1_dist_resid", "C2_dist_resid", "C3_dist_resid")),
    names_to = "distance_type", values_to = "distance_value"
  ) %>%
  dplyr::mutate(
    detrended = ifelse(
      grepl("resid", distance_type), 
      "detrended (residuals)", "not detrended"
    ),
    distance_type = sub("_resid", "", distance_type),
    distance_type = sub("_dist", "", distance_type),
    distance_type = factor(distance_type, levels = c("C1", "C2", "C3"))
  ) %>%
  dplyr::mutate(
    detrended = factor(detrended, levels = c("not detrended", "detrended (residuals)"))
  )

# plot loop
ps <- lapply(
  d_binned_long %>% 
    dplyr::filter(detrended == "detrended (residuals)") %>%
    dplyr::group_split(distance_type), 
  
  function(x) {
    
    ggplot(x) + 
      geom_raster(
        aes(
          x = geo_dist_cut,
          y = time_dist_cut,
          fill = distance_value
        )
      ) +
      facet_grid(cols = dplyr::vars(distance_type)) +
      scale_fill_viridis_c(direction = -1) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7, angle = 45, hjust = 0.9)
      ) +
      guides(
        fill = guide_colorbar(title = "distance:", barwidth = 6)
      ) +
      xlab("spatial distance: 100km bins") +
      ylab("temporal distance: 100y bins")
    
  })

cowplot::plot_grid(plotlist = ps, nrow = 2, ncol = 2)
