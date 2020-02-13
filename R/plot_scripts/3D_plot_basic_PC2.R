library(magrittr)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered

threed <- anno %>%
  dplyr::transmute(
    x = x/1000, 
    y = y/1000,
    z = calage_center,
    color = viridis::plasma(50)[as.numeric(cut(PC2, breaks = 50))]
  )

# plot
png(filename = "plots/3D_plot_basic_PC2.png", width = 22, height = 18, units = "cm", res = 300)

s <- scatterplot3d::scatterplot3d(
  threed$x, threed$y, threed$z, color = threed$color,
  cex.symbols = 0.7, angle = 70,
  xlab = "x", ylab = "y", zlab = "time calBC",
  col.axis = "grey",
  zlim = c(-7500, -500)
)

dev.off()
