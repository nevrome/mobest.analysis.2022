library(magrittr)
library(ggplot2)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final

load("data/gpr/interpol_grid_spatial.RData")
load("data/gpr/interpol_grid.RData")

load("data/timepillars/poi.RData")

# pois
toi <- lapply(
  1:nrow(poi), function(i) {
    dm <- sf::st_distance(interpol_grid_spatial, poi[i,])
    interpol_grid_spatial[which(min(dm) == dm),]
  }
)

toi_dots <- lapply(
  toi, function(t) {
    tibble::tibble(
      x = t$x[1],
      y = t$y[1]
    )
  }
) %>% dplyr::bind_rows()

names(toi) <- poi$poi_id
toi_dots$poi_id <- poi$poi_id

#### filter ####
poi_timeseries <- interpol_grid %>%
  dplyr::filter(
    dependent_var_id %in% c("C1", "C2"),
    # x == toi_dots$x[2],
    # y == toi_dots$y[2],
    #z %% 500 == 0
  ) %>% 
  tidyr::pivot_wider(
    id_cols = z,
    names_from = "dependent_var_id",
    values_from = c("mean", "sd")
  )

#### plot ####
ggplot() +
  geom_point(
    data = anno,
    aes(x = C1, y = C2, color = region_id, shape = age_group_id),
    alpha = 0.7,
    size = 2
  ) +
  scale_color_manual(
    values = c(
      "#999999", "#E69F00", "#56B4E9", "#009E73", "#871200",
      "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#2fff00"
    )
  ) +
  guides(
    color = guide_legend()
  ) +
  ggnewscale::new_scale_color() +
  geom_path(
    data = poi_timeseries,
    aes(x = mean_C1, y = mean_C2)
  ) +
  geom_errorbar(
    data = poi_timeseries,
    aes(
      x = mean_C1, 
      ymin = mean_C2 - sd_C2, ymax = mean_C2 + sd_C2,
      color = z
    ),
    alpha = 0.5
  ) +
  geom_errorbarh(
    data = poi_timeseries,
    aes(
      y = mean_C2, 
      xmin = mean_C1 - sd_C1, xmax = mean_C1 + sd_C1,
      color = z
    ),
    alpha = 0.5
  ) +
  geom_point(
    data = poi_timeseries,
    aes(
      x = mean_C1, 
      y = mean_C2,
      color = z
    ),
    size = 3
  ) +
  scale_color_gradient2(
    limits = c(-7500, -500), low = "black", mid = "red", high = "green", midpoint = -5000,
    breaks = seq(-7500, -500, 1000)
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "right"
  ) +
  scale_shape_manual(
    values = c(
      ">-10000" = 15,
      "-8000 - -6000" = 15,
      "-6000 - -4000" = 17,
      "-4000 - -2000" = 6,
      "-2000 - 0" = 4
    )
  ) +
  coord_fixed() +
  scale_y_reverse()

