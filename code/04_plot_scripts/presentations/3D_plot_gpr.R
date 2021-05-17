library(magrittr)

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")

model_grid <- mobest::create_model_grid(
  independent = mobest::create_spatpos_multi(
    id = janno_final$Individual_ID,
    x = list(janno_final$x),
    y = list(janno_final$y),
    z = list(janno_final$Date_BC_AD_Median_Derived),
    it = "age_median"
  ),
  dependent = mobest::create_obs(
    C1 = janno_final$C1
  ),
  kernel = mobest::create_kernset_multi(
    d = list(c(600000, 600000, 900)), 
    g = 0.06, 
    on_residuals = T, 
    auto = F,
    it = "ds500_dt800_g008"
  ),
  prediction_grid = list(
    scs100_tl50 = mobest::prediction_grid_for_spatiotemporal_area(
      area,
      spatial_cell_size = 150000,
      temporal_layers = seq(-8000, 2000, 200)
    )
  )
)

interpol_grid <- mobest::run_model_grid(model_grid)

threed <- janno_final %>%
  dplyr::transmute(
    x = x/1000, 
    y = y/1000,
    z = Date_BC_AD_Median_Derived,
    color = viridis::viridis(50)[
      as.numeric(cut(janno_final$C1, breaks = 50))
    ]
  )

threedinter <- interpol_grid %>%
  dplyr::mutate(
    order = findInterval(mean, sort(mean)),
    mean_limited = ifelse(
      mean < min(janno_final$C1), min(janno_final$C1), 
      ifelse(
        mean > max(janno_final$C1), max(janno_final$C1),
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
    alpha = (1 - (sd - min(sd)) / (max(sd) - min(sd))) * 0.9
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
  col = ggplot2::alpha(threedinter$color, threedinter$alpha)
)

dev.off()

