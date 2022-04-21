library(magrittr)

load("data/genotype_data/janno_final.RData")
load("data/spatial/area.RData")

#### basic plot of observations ####

threed <- janno_final %>%
  dplyr::transmute(
    x = x/1000, 
    y = y/1000,
    z = Date_BC_AD_Median_Derived,
    color = viridis::viridis(50)[as.numeric(cut(C1_mds_u, breaks = 50))]
  )

# plot
png(
  filename = "plots/presentation/3D_plot_basic_C1.png", 
  width = 22, height = 14, units = "cm", res = 300
)

scatterplot3d::scatterplot3d(
  threed$x, threed$y, threed$z, color = threed$color,
  pch = 18, cex.symbols = 1.5,
  angle = 70,
  xlab = "x", ylab = "y", zlab = "time calBC/AD",
  col.axis = "grey",
  zlim = c(-8000, 2000),
  mar = c(2.7, 2.7, 0, 2)
)

dev.off()

#### plot with interpolation ####

model_grid <- mobest::create_model_grid(
  independent = mobest::create_spatpos_multi(
    age_median = mobest::create_spatpos(
      id = janno_final$Poseidon_ID,
      x = janno_final$x,
      y = janno_final$y,
      z = janno_final$Date_BC_AD_Median_Derived
    )
  ),
  dependent = mobest::create_obs(
    C1_mds_u = janno_final$C1_mds_u
  ),
  kernel = mobest::create_kernset_multi(
    kernel1 = mobest::create_kernset(
      C1_mds_u = mobest::create_kernel(500000, 500000, 900, 0.06)
    )
  ),
  prediction_grid = mobest::create_spatpos_multi(
    timeslice = mobest::create_prediction_grid(
      area,
      spatial_cell_size = 50000
    ) %>% mobest::geopos_to_spatpos(seq(-8000, 2000, 200))
  )
)

interpol_grid <- mobest::run_model_grid(model_grid)

threedinter <- interpol_grid %>%
  dplyr::mutate(
    order = findInterval(mean, sort(mean)),
    mean_limited = ifelse(
      mean < min(janno_final$C1_mds_u), min(janno_final$C1_mds_u), 
      ifelse(
        mean > max(janno_final$C1_mds_u), max(janno_final$C1_mds_u),
        mean     
      )
    )
  ) %>%
  dplyr::transmute(
    x = x/1000, 
    y = y/1000,
    z = z,
    color = viridis::viridis(50)[
      as.numeric(cut(mean_limited, breaks = 50))
    ],
    alpha = (1 - (sd - min(sd)) / (max(sd) - min(sd))) * 0.9,
    mean = mean,
    sd = sd
  )

# plot
png(filename = "plots/presentation/3D_plot_gpr_C1.png", width = 22, height = 14, units = "cm", res = 300)

s <- scatterplot3d::scatterplot3d(
  threed$x, threed$y, threed$z, color = threed$color,
  pch = 18, cex.symbols = 1.6,
  angle = 70,
  xlab = "x", ylab = "y", zlab = "time calBC/AD",
  col.axis = "grey",
  zlim = c(-8000, 2000),
  mar = c(2.7, 2.7, 0, 2)
)

cs <- s$xyz.convert(threedinter$x, threedinter$y, threedinter$z)
points(
  cs$x, cs$y, lwd = 0.1, pch = 18, cex = 0.8,
  col = ggplot2::alpha(threedinter$color, threedinter$alpha * 0.2)
)

dev.off()

#### plot with one interpolation timeslice ####

threedinter_timeslice <- threedinter %>%
  dplyr::filter(z == -5000)

# plot
png(filename = "plots/presentation/3D_plot_gpr_C1_timeslice.png", width = 22, height = 14, units = "cm", res = 300)

s <- scatterplot3d::scatterplot3d(
  threedinter_timeslice$x, threedinter_timeslice$y, threedinter_timeslice$z, 
  color = ggplot2::alpha(threedinter_timeslice$color, threedinter_timeslice$alpha),
  xlim = range(threed$x), ylim = range(threed$y),
  lwd = 0.1, pch = 18, cex.symbols = 0.8,
  angle = 70,
  xlab = "x", ylab = "y", zlab = "time calBC/AD",
  col.axis = "grey",
  zlim = c(-8000, 2000),
  mar = c(2.7, 2.7, 0, 2)
)

dev.off()

#### plot with one timeslice, z-axis C1 ####

png(filename = "plots/presentation/3D_plot_gpr_C1_timeslice_zC1.png", width = 22, height = 14, units = "cm", res = 300)

s <- scatterplot3d::scatterplot3d(
  threedinter_timeslice$x, threedinter_timeslice$y, threedinter_timeslice$mean, 
  color = ggplot2::alpha(threedinter_timeslice$color, threedinter_timeslice$alpha),
  xlim = range(threed$x), ylim = range(threed$y),
  lwd = 0.1, pch = 18, cex.symbols = 0.8,
  angle = 70,
  xlab = "x", ylab = "y", zlab = "MDS C1",
  col.axis = "grey",
  zlim = c(-0.08, 0.11),
  mar = c(2.7, 2.7, 0, 2)
)

dev.off()

#### plot with one timeslice, z-axis C1 and error ####

png(filename = "plots/presentation/3D_plot_gpr_C1_timeslice_zC1_sd.png", width = 22, height = 14, units = "cm", res = 300)

s <- scatterplot3d::scatterplot3d(
  threedinter_timeslice$x, threedinter_timeslice$y, threedinter_timeslice$mean, 
  color = ggplot2::alpha(threedinter_timeslice$color, threedinter_timeslice$alpha),
  xlim = range(threed$x), ylim = range(threed$y),
  lwd = 0.1, pch = 18, cex.symbols = 0.8,
  angle = 70,
  xlab = "x", ylab = "y", zlab = "MDS C1",
  col.axis = "grey",
  zlim = c(-0.08, 0.11),
  mar = c(2.7, 2.7, 0, 2)
)

top <- s$xyz.convert(
  threedinter_timeslice$x + threedinter_timeslice$sd,
  threedinter_timeslice$y + threedinter_timeslice$sd,
  threedinter_timeslice$mean + threedinter_timeslice$sd
)
bottom <- s$xyz.convert(
  threedinter_timeslice$x - threedinter_timeslice$sd,
  threedinter_timeslice$y - threedinter_timeslice$sd,
  threedinter_timeslice$mean - threedinter_timeslice$sd
)
segments(
  top$x, top$y, bottom$x, bottom$y,
  col = ggplot2::alpha(threedinter_timeslice$color, threedinter_timeslice$alpha * 1.5),
  lwd = 0.5
)

dev.off()
