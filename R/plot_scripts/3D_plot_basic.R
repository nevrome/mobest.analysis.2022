library(magrittr)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered

threed <- anno %>%
  dplyr::transmute(
    x = x/1000, 
    y = y/1000,
    z = calage_center,
    color = viridis::viridis(50)[as.numeric(cut(PC1, breaks = 50))]
  )

# plot
png(filename = "plots/3D_plot_basic_PC1.png", width = 22, height = 18, units = "cm", res = 300)

scatterplot3d::scatterplot3d(
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

dev.off()
