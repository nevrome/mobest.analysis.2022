library(magrittr)
library(ggplot2)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered
load("data/gpr/pred_grid_filled_grouped.RData")
pri <- pred_grid_filled_grouped %>% dplyr::filter(
    independent_table_type == "age_center", kernel_setting_id == "ds200_dt400_g01"
  ) %>%
  tidyr::pivot_wider(names_from = "dependent_var_id", values_from = c("mean", "sd")) %>%
  dplyr::select(x_real, y_real, age_sample, mean_PC1, sd_PC1)

min_PC1 <- min(c(anno$PC1, pri$mean_PC1))
max_PC1 <- max(c(anno$PC1, pri$mean_PC1))
PC1_seq <- seq(min_PC1, max_PC1, length.out = 50)

threed <- anno %>%
  dplyr::transmute(
    x = x/1000, 
    y = y/1000,
    z = calage_center,
    color = viridis::viridis(50)[as.numeric(cut(PC1, breaks = PC1_seq))]
  )

threedinter <- pri %>%
  dplyr::mutate(
    order = findInterval(mean_PC1, sort(mean_PC1))
  ) %>%
  dplyr::transmute(
    x = x_real/1000, 
    y = y_real/1000,
    z = age_sample,
    color = viridis::viridis(50)[as.numeric(cut(mean_PC1, breaks = PC1_seq))],
    alpha = sd_PC1
  )

# plot
png(filename = "plots/3D_plot_gpr.png", width = 22, height = 18, units = "cm", res = 300)

s <- scatterplot3d::scatterplot3d(
  threed$x, threed$y, threed$z, color = threed$color, 
  cex.symbols = 1.5, 
  angle = 70,
  xlab = "x [km]", ylab = "y [km]", zlab = "time calBC [y]",
  cex.axis = 0.8,
  cex.lab = 1.5,
  col.axis = "grey",
  zlim = c(-7500, -500),
  mar = c(3,3,0,2)
)

#### gpr ####
cs <- s$xyz.convert(threedinter$x, threedinter$y, threedinter$z)
points(
  cs$x, cs$y, lwd = 0.1, pch = 18,
  col = alpha(threedinter$color, 0.1)#col = alpha(threedinter$color, 0.5 - threedinter$alpha)
)

dev.off()
