library(magrittr)

load("data/tessellation/tessellation_3D_data_burial_type.RData")
load("data/spatial/research_area.RData")
raps <- sf::st_coordinates(sf::st_cast(research_area, "POINT"))[1:4,]/1000

threed <- ver %>%
  dplyr::transmute(
    x = x/1000, 
    y = y/1000,
    z = z
  )

polygon_edges$x.a <- polygon_edges$x.a/1000
polygon_edges$y.a <- polygon_edges$y.a/1000
polygon_edges$x.b <- polygon_edges$x.b/1000
polygon_edges$y.b <- polygon_edges$y.b/1000

# plot
png(filename = "plots/3D_plot_tessellation.png", width = 22, height = 18, units = "cm", res = 300)

s <- scatterplot3d::scatterplot3d(
  threed$x, threed$y, threed$z, color = "red",
  cex.symbols = 0.7, angle = 70,
  xlab = "x", ylab = "y", zlab = "time calBC",
  col.axis = "grey",
  zlim = c(-7500, -500)
)

#### tesselation ####
csstart <- s$xyz.convert(polygon_edges[[1]], polygon_edges[[2]], polygon_edges[[3]])
csstop <- s$xyz.convert(polygon_edges[[4]], polygon_edges[[5]], polygon_edges[[6]])
for(i in 1:length(csstart$x)) {
  segments(csstart$x[i], csstart$y[i], csstop$x[i], csstop$y[i], lwd = 0.1, col = "black")
}

dev.off()
