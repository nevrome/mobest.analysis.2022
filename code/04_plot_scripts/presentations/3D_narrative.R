library(magrittr)

load("data/genotype_data/janno_final.RData")
load("data/spatial/area.RData")
load("data/spatial/search_area.RData")

janno_search <- janno_final %>%
  dplyr::mutate(
    search_z = Date_BC_AD_Median_Derived
  ) %>% dplyr::filter(
    Poseidon_ID %in% c(
      "Stuttgart_published.DG"
    )
  )

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

s <- scatterplot3d::scatterplot3d(
  threed$x, threed$y, threed$z, color = threed$color,
  pch = 18, cex.symbols = 1.5,
  angle = 70,
  xlab = "x", ylab = "y", zlab = "time calBC/AD",
  col.axis = "grey",
  zlim = c(-8000, 2000),
  mar = c(2.7, 2.7, 0, 2)
)

s$box3d()
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
      C1_mds_u = mobest::create_kernel(800000, 800000, 800, 0.07)
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

s$box3d()
dev.off()

#### plot with one interpolation timeslice ####

threedinter_timeslice <- threedinter %>%
  dplyr::filter(z == -5600)

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

s$box3d()
dev.off()

#### plot with one timeslice, z-axis C1, old angle ####

png(filename = "plots/presentation/3D_plot_gpr_C1_timeslice_zC1a.png", width = 22, height = 14, units = "cm", res = 300)

s <- scatterplot3d::scatterplot3d(
  threedinter_timeslice$x, threedinter_timeslice$y, threedinter_timeslice$mean, 
  color = threedinter_timeslice$color,#ggplot2::alpha(threedinter_timeslice$color, threedinter_timeslice$alpha),
  xlim = range(threed$x), ylim = range(threed$y),
  lwd = 0.1, pch = 18, cex.symbols = 0.8,
  angle = 70,
  xlab = "x", ylab = "y", zlab = "MDS C1",
  col.axis = "grey",
  zlim = c(-0.11, 0.11),
  mar = c(2.7, 2.7, 0, 2)
)

s$box3d()
dev.off()

#### plot with one timeslice, z-axis C1 ####

png(filename = "plots/presentation/3D_plot_gpr_C1_timeslice_zC1b.png", width = 22, height = 14, units = "cm", res = 300)

s <- scatterplot3d::scatterplot3d(
  threedinter_timeslice$x, threedinter_timeslice$y, threedinter_timeslice$mean, 
  color = threedinter_timeslice$color,#ggplot2::alpha(threedinter_timeslice$color, threedinter_timeslice$alpha),
  xlim = range(threed$x), ylim = range(threed$y),
  lwd = 0.1, pch = 18, cex.symbols = 0.8,
  angle = -70,
  xlab = "x", ylab = "y", zlab = "MDS C1",
  col.axis = "grey",
  zlim = c(-0.11, 0.11),
  mar = c(2.7, 2.0, 0, 2.7)
)

s$box3d()
dev.off()

#### plot with one timeslice, z-axis C1 and error ####

cut_segments <- function(x, y, z_top, z_bottom, color ) {
  top <- s$xyz.convert(x, y, z_top)
  bottom <- s$xyz.convert(x, y, z_bottom)
  segments(
    bottom$x, bottom$y, top$x, top$y,
    col = color, lwd = 0.8
  )
}

png(filename = "plots/presentation/3D_plot_gpr_C1_timeslice_zC1b_error.png", width = 22, height = 14, units = "cm", res = 300)

s <- scatterplot3d::scatterplot3d(
  threedinter_timeslice$x, threedinter_timeslice$y, threedinter_timeslice$mean, 
  color = threedinter_timeslice$color,#ggplot2::alpha(threedinter_timeslice$color, threedinter_timeslice$alpha),
  xlim = range(threed$x), ylim = range(threed$y),
  lwd = 0.1, pch = 18, cex.symbols = 0.8,
  angle = -70,
  xlab = "x", ylab = "y", zlab = "MDS C1",
  col.axis = "grey",
  zlim = c(-0.11, 0.11),
  mar = c(2.7, 2.0, 0, 2.7)
)

cut_segments(
  threedinter_timeslice$x,
  threedinter_timeslice$y,
  threedinter_timeslice$mean + threedinter_timeslice$sd,
  threedinter_timeslice$mean - threedinter_timeslice$sd,
  threedinter_timeslice$color
)

s$box3d()
dev.off()

#### plot with one timeslice, z-axis C1, observation plane ####

obsval <- janno_search$C1_mds_u

threedinter_timeslice_below <- threedinter_timeslice %>%
  dplyr::filter(mean < obsval)
threedinter_timeslice_above <- threedinter_timeslice %>%
  dplyr::filter(mean >= obsval)

seglist <- threedinter_timeslice %>%
  dplyr::mutate(
    segment_position = dplyr::case_when(
      mean-sd >= obsval ~ "01_fully_above",
      mean+sd < obsval ~ "02_fully_below",
      TRUE ~ "03_cut"
    )
  ) %>% dplyr::group_split(segment_position)

png(filename = "plots/presentation/3D_plot_gpr_C1_timeslice_zC1b_plane.png", width = 22, height = 14, units = "cm", res = 300)

s <- scatterplot3d::scatterplot3d(
  threedinter_timeslice_below$x, threedinter_timeslice_below$y, threedinter_timeslice_below$mean, 
  color = threedinter_timeslice_below$color,#ggplot2::alpha(threedinter_timeslice$color, threedinter_timeslice$alpha),
  xlim = range(threed$x), ylim = range(threed$y),
  lwd = 0.1, pch = 18, cex.symbols = 0.8,
  angle = -70,
  xlab = "x", ylab = "y", zlab = "MDS C1",
  col.axis = "grey",
  zlim = c(-0.11, 0.11),
  mar = c(2.7, 2.0, 0, 2.7)
)

below <- s$xyz.convert(
  threedinter_timeslice_below$x,
  threedinter_timeslice_below$y,
  threedinter_timeslice_below$mean
)
points(
  below$x, below$y,
  col = threedinter_timeslice_below$color,
  pch = 18, cex = 0.8
)
cut_segments(
  seglist[[2]]$x, seglist[[2]]$y,
  seglist[[2]]$mean + seglist[[2]]$sd,
  seglist[[2]]$mean - seglist[[2]]$sd,
  seglist[[2]]$color
)
cut_segments(
  seglist[[3]]$x, seglist[[3]]$y,
  rep(obsval, nrow(seglist[[3]])),
  seglist[[3]]$mean - seglist[[3]]$sd,
  seglist[[3]]$color
)

s$plane3d(
  Intercept = obsval,#janno_search$C1_mds_u,
  x.coef = 0, y.coef = 0,
  lty = "dotted", draw_polygon = TRUE, draw_lines = TRUE, 
  polygon_args = list(col = rgb(0.8, 0.8, 0.8, 0.8))
)

above <- s$xyz.convert(
  threedinter_timeslice_above$x,
  threedinter_timeslice_above$y,
  threedinter_timeslice_above$mean
)
points(
  above$x, above$y,
  col = threedinter_timeslice_above$color,
  pch = 18, cex = 0.8
)
cut_segments(
  seglist[[1]]$x, seglist[[1]]$y,
  seglist[[1]]$mean + seglist[[1]]$sd,
  seglist[[1]]$mean - seglist[[1]]$sd,
  seglist[[1]]$color
)
cut_segments(
  seglist[[3]]$x, seglist[[3]]$y,
  seglist[[3]]$mean + seglist[[3]]$sd,
  rep(obsval, nrow(seglist[[3]])),
  seglist[[3]]$color
)

s$box3d()
dev.off()

#### origin search ####

spatial_pred_grid <- mobest::create_prediction_grid(
  area,
  spatial_cell_size = 30000
)

search <- mobest::locate(
  independent = mobest::create_spatpos(
    id = janno_final$Poseidon_ID,
    x = janno_final$x,
    y = janno_final$y,
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = mobest::create_obs(
    C1_mds_u = janno_final$C1_mds_u,
    C2_mds_u = janno_final$C2_mds_u
  ),
  kernel = mobest::create_kernset(
    C1_mds_u = mobest::create_kernel(800000, 800000, 800, 0.07),
    C2_mds_u = mobest::create_kernel(800000, 800000, 800, 0.07)
  ),
  search_independent = mobest::create_spatpos(
    id = janno_search$Poseidon_ID,
    x = janno_search$x,
    y = janno_search$y,
    z = janno_search$Date_BC_AD_Median_Derived
  ),
  search_dependent = mobest::create_obs(
    C1_mds_u = janno_search$C1_mds_u,
    C2_mds_u = janno_search$C2_mds_u
  ),
  search_space_grid = spatial_pred_grid,
  search_time = -5600,
  search_time_mode = "absolute"
)

search_prod <- mobest::multiply_dependent_probabilities(search)

# threedprob <- search_prod %>%
#   dplyr::mutate(
#     x = field_x/1000,
#     y = field_y/1000,
#     z = probability_product,
#     color = viridis::inferno(
#       50, direction = -1
#     )[as.numeric(cut(probability_product, breaks = 50))]
#   )
# 
# png(filename = "plots/presentation/3D_plot_prob_Stuttgart.png", width = 22, height = 14, units = "cm", res = 300)
# 
# s <- scatterplot3d::scatterplot3d(
#   threedprob$x, threedprob$y, threedprob$z, 
#   color = threedprob$color,
#   xlim = range(threed$x), ylim = range(threed$y),
#   lwd = 0.5, pch = 18, cex.symbols = 0.8,
#   angle = -70,
#   xlab = "x", ylab = "y", zlab = "MDS C1",
#   col.axis = "grey",
#   zlim = c(0, 2000),
#   mar = c(2.7, 2.0, 0, 2.7)
# )
# 
# s$box3d()
# dev.off()
#   
  
library(ggplot2)

load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")

plot_prob <- function(search_prod, path) {

  p <- ggplot() +
    geom_sf(data = extended_area, fill = "black") +
    geom_raster(
      data = search_prod,
      mapping = aes(x = field_x, y = field_y, fill = probability_product),
    ) +
    scale_fill_viridis_c(option = "mako", direction = -1) +
    geom_sf(data = extended_area, fill = NA, colour = "black") +
    geom_point(
      data = janno_search,
      mapping = aes(x = x, y = y),
      colour = "red",
      size = 5
    ) +
    theme_bw() +
    coord_sf(
      expand = FALSE,
      crs = epsg3035
    ) +
    guides(
      fill = guide_colorbar(title = "Probability  ", barwidth = 25)
    ) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = 15),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.text = element_text(size = 15),
      strip.text = element_text(size = 15),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "#BFD5E3")
    ) +
    ggtitle(paste("time calBC/AD:", unique(search_prod$search_z)))
  
  ggsave(
    path,
    plot = p,
    device = "png",
    scale = 0.6,
    dpi = 300,
    width = 350, height = 300, units = "mm",
    limitsize = F
  )

}

plot_prob(search_prod, "plots/presentation/search_map.png")

#### movie ####

search_movie <- mobest::locate(
  independent = mobest::create_spatpos(
    id = janno_final$Poseidon_ID,
    x = janno_final$x,
    y = janno_final$y,
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = mobest::create_obs(
    C1_mds_u = janno_final$C1_mds_u,
    C2_mds_u = janno_final$C2_mds_u
  ),
  kernel = mobest::create_kernset(
    C1_mds_u = mobest::create_kernel(800000, 800000, 800, 0.07),
    C2_mds_u = mobest::create_kernel(800000, 800000, 800, 0.07)
  ),
  search_independent = mobest::create_spatpos(
    id = janno_search$Poseidon_ID,
    x = janno_search$x,
    y = janno_search$y,
    z = janno_search$Date_BC_AD_Median_Derived
  ),
  search_dependent = mobest::create_obs(
    C1_mds_u = janno_search$C1_mds_u,
    C2_mds_u = janno_search$C2_mds_u
  ),
  search_space_grid = spatial_pred_grid,
  search_time = seq(-8000, -5000, 50),
  search_time_mode = "absolute"
)

search_movie_prod <- mobest::multiply_dependent_probabilities(search_movie)

search_movie_prod %>%
  dplyr::group_split(search_z) %>%
  purrr::walk2(
    ., paste0("plots/presentation/movie/", sprintf("frame_%03d", 1:length(.))),
    plot_prob
  )
