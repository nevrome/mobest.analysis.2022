library(magrittr)
library(laGP)
source("R/helper_functions.R")

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/area.RData")
bb <- unname(sf::st_bbox(research_area))

#### prep independent variables with temporal sampling ####

independent_list <- lapply(
  1:10,#1:length(anno$calage_sample[[1]]), 
  function(i, anno) {
    age_sample <- sapply(anno$calage_sample, function(x){ x[i] })
    dplyr::transmute(
      .data = anno,
      x_01 = range_01_x(x),
      y_01 = range_01_y(y),
      z_01 = range_01_z(age_sample)
    )
  },
  anno
)

#### create prediction grid ####

pred_points_space <- area %>% 
  sf::st_make_grid(cellsize = 100000, what = "centers") %>%
  sf::st_intersection(area) %>%
  sf::st_coordinates() %>%
  tibble::as_tibble() %>%
  dplyr::rename(x_real = X, y_real = Y)

time_layers <- tibble::tibble(
  age_sample = seq(-7500, -500, 500)
)

pred_grid <- pred_points_space %>% 
  tidyr::crossing(time_layers) %>%
  dplyr::mutate(
    x_01 = range_01_x(x_real),
    y_01 = range_01_y(y_real),
    z_01 = range_01_z(age_sample)
  )

#### gp regression ####

predictgp <- function(independent, dependent, pred_grid) {
  # priors for the global GP
  da <- darg(list(mle = TRUE, max=10), independent)
  ga <- garg(list(mle = TRUE, max=10), dependent)
  # fit the global GP
  gp <- newGPsep(
    X = independent, Z = dependent, 
    d = da$start, g = ga$start,
    dK = TRUE
  )
  mleGPsep(
    gpsepi = gp, 
    param = "both", 
    tmin = c(da$min, ga$min), tmax = c(da$max, ga$max), ab = c(da$ab, ga$ab), 
    maxit = 200
  )
  # predictions from the global GP on the prediction
  pred <- predGPsep(gp, XX = pred_grid[, c("x_01", "y_01", "z_01")], lite = T)
  # delete GP object
  deleteGPsep(gp)
  # return result 
  return(pred)
}

prediction_list <- lapply(independent_list, function(x) {
  predictgp(x, anno$PC1, pred_grid)
})

prediction_sample_list <- lapply(prediction_list, function(x) {
    pred_sample = sapply(1:length(x$mean), function(i) { rnorm(1, x$mean[i], sqrt(x$s2[i])) })
})

prediction_sample_df <- lapply(1:length(prediction_sample_list), function(i) {
  data.frame(
    run_id = i,
    point_id = 1:length(prediction_sample_list[[i]]),
    pred_samples = prediction_sample_list[[i]]
  )
}) %>% dplyr::bind_rows() %>% tibble::as_tibble()

prediction_per_point_df <- prediction_sample_df %>%
  dplyr::group_by(point_id) %>%
  dplyr::summarize(mean = mean(pred_samples), sd = sd(pred_samples))


pred_grid$pred_PC1_mean <- prediction_per_point_df$mean
pred_grid$pred_PC1_sd <- prediction_per_point_df$sd

pred_grid_spatial_cropped <- sf::st_as_sf(pred_grid, coords = c("x_real", "y_real"), crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs") %>%
  #sf::st_intersection(research_area) %>%
  dplyr::mutate(
    x_real = sf::st_coordinates(.)[,1],
    y_real = sf::st_coordinates(.)[,2]
  )




