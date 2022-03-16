library(magrittr)

load("data/genotype_data/janno_final.RData")

threed <- janno_final %>%
  dplyr::transmute(
    x = x/1000, 
    y = y/1000,
    z = Date_BC_AD_Median_Derived,
    color = viridis::viridis(50)[as.numeric(cut(C1, breaks = 50))]
  )

# plot
png(filename = "plots/presentation/3D_plot_basic_C1.png", width = 22, height = 14, units = "cm", res = 300)

s <- scatterplot3d::scatterplot3d(
  threed$x, threed$y, threed$z, color = threed$color,
  pch = 18, cex.symbols = 1.5,
  angle = 70,
  xlab = "x", ylab = "y", zlab = "time calBC/AD",
  col.axis = "grey",
  zlim = c(-8000, 2000),
  mar = c(2.7, 2.7, 0, 2)
)

dev.off()
