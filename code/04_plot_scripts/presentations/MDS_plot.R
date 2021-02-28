library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")
load("data/plot_reference_data/region_id_shapes.RData")
load("data/plot_reference_data/age_colors_gradient.RData")

# mean per region and time
region_age_group_mean <- janno_final %>%
  dplyr::group_by(region_id, age_group_id) %>%
  dplyr::summarise(mean_C1 = mean(C1), mean_C2 = mean(C2), z = mean(Date_BC_AD_Median_Derived)) %>%
  dplyr::ungroup()

# grid arrangement for mean points
C1_grid <- seq(min(janno_final$C1), max(janno_final$C1), length.out = 35)
C2_grid <- seq(min(janno_final$C2), max(janno_final$C2), length.out = 36)
C_grid <- expand.grid(C1_grid, C2_grid)
distance_matrix <- fields::rdist(C_grid, region_age_group_mean[c("mean_C1", "mean_C2")])
distance_long <- setNames(reshape2::melt(distance_matrix), c('grid_id', 'mean_point_id', 'distance'))

arrange_on_grid <- function(distance_long, grid_df = data.frame()) {
  # sort table by distance
  distance_long_sorted <- distance_long[order(distance_long$distance),]
  # get smallest distance grid point by input point
  closest_grid_points <- distance_long_sorted[!duplicated(distance_long_sorted$mean_point_id),]
  # find grid point duplicates and create subsets with already uniquely claimed positions
  dups <- unique(closest_grid_points$grid_id[duplicated(closest_grid_points$grid_id)])
  without_dups <- closest_grid_points[!(closest_grid_points$grid_id %in% dups),]
  with_dups <- closest_grid_points[closest_grid_points$grid_id %in% dups,]
  # find best input point matches according to distance for duplicated grid points
  with_dups_better <- with_dups[!duplicated(with_dups$grid_id),]
  # construct current version of the output grid
  grid_df <- unique(rbind(grid_df, without_dups, with_dups_better))
  # construct distance table with yet unattributed input and grid points
  distance_long <- distance_long[
    !(distance_long$grid_id %in% grid_df$grid_id) &
      !(distance_long$mean_point_id %in% grid_df$mean_point_id),
  ]
  # make decision whether the attribution is ready or not
  if (nrow(distance_long) == 0) { 
    return(grid_df)
  } else {
    arrange_on_grid(distance_long, grid_df)
  }
}

grid_df <- arrange_on_grid(distance_long)

region_age_group_mean[grid_df$mean_point_id,c("mean_C1", "mean_C2")] <- C_grid[grid_df$grid_id,]

# normal mds plot
p_mds <- ggplot() +
  geom_point(
    data = janno_final,
    aes(x = C1, y = C2, color = Date_BC_AD_Median_Derived, shape = region_id),
    alpha = 0.5,
    size = 2
  ) +
  geom_point(
    data = region_age_group_mean,
    aes(x = mean_C1, y = mean_C2),
    size = 4,
    fill = "white",
    color = "black",
    shape = 21
  ) +
  geom_point(
    data = region_age_group_mean,
    aes(x = mean_C1, y = mean_C2, color = z, shape = region_id),
    size = 2
  ) +
  scale_shape_manual(
    values = region_id_shapes
  ) +
  age_colors_gradient +
  coord_fixed(xlim = c(-0.1, 0.05), ylim = c(-0.09, 0.065)) +
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  scale_x_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.title = element_text(size = 13),
    legend.spacing.y = unit(0.2, 'cm'),
    legend.key.height = unit(0.4, 'cm'),
    legend.text = element_text(size = 10),
  ) +
  guides(
    color = guide_colorbar(title = "Time", barwidth = 20, barheight = 1.5),
    shape = guide_legend(title = "Region", nrow = 3, ncol = 4, byrow = F)
  )

ggsave(
  paste0("plots/MDS_plot.jpeg"),
  plot = p_mds,
  device = "jpeg",
  scale = 0.3,
  dpi = 300,
  width = 1000, height = 600, units = "mm",
  limitsize = F
)
