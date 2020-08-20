library(magrittr)

load("data/poseidon_data/janno_final.RData")
load("data/gpr/interpol_grid.RData")
inter <- interpol_grid %>%
  dplyr::filter(
    dependent_var_id == "C1",
    independent_table_id == "age_sample_2", 
    kernel_setting_id == "ds800_dt1400_g001",
    pred_grid_id == "scs100_tl100"
  )


threed <- janno_final %>%
  dplyr::transmute(
    x = x/1000, 
    y = y/1000,
    z = Date_BC_AD_Median_Derived,
    color = viridis::viridis(50)[as.numeric(cut(C1, breaks = 50))]
  )

threedinter <- inter %>%
  dplyr::mutate(
    order = findInterval(mean, sort(mean))
  ) %>%
  dplyr::transmute(
    x = x/1000, 
    y = y/1000,
    z = z,
    color = viridis::viridis(50)[as.numeric(cut(mean, breaks = 50))],
    alpha = (1 - (sd - min(sd)) / (max(sd) - min(sd))) / 4
  )

# plot
png(filename = "plots/3D_plot_gpr_C1.png", width = 22, height = 14, units = "cm", res = 300)

s <- scatterplot3d::scatterplot3d(
  threed$x, threed$y, threed$z, color = threed$color,
  pch = 18, cex.symbols = 1.5,
  angle = 70,
  xlab = "x", ylab = "y", zlab = "time calBC/AD",
  col.axis = "grey",
  zlim = c(-8000, 2000),
  mar = c(2.7, 2.7, 0, 2)
)

cs <- s$xyz.convert(threedinter$x, threedinter$y, threedinter$z)
points(
  cs$x, cs$y, lwd = 0.1, pch = 18, cex = 0.8,
  col = ggplot2::alpha(threedinter$color, threedinter$alpha)
)

dev.off()

