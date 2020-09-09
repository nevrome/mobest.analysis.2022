library(magrittr)
library(ggplot2)

range_01 <- function(x, min, max) { (x - min) / (max - min) }
dist_scale_01 <- function(x, min, max) { x / abs(min - max) }
range_real <- function(x, min, max) { min + x * abs(min - max) }
dist_scale_real <- function(x, min, max) { x * abs(min - max) }

range_01_x <- function(x, start = min(janno_final$x), stop = max(janno_final$x)) { range_01(x, start, stop) }
range_01_y <- function(y, start = min(janno_final$y), stop = max(janno_final$y)) { range_01(y, start, stop) }
range_01_z <- function(z, start = min(janno_final$Date_BC_AD_Median_Derived), stop = max(janno_final$Date_BC_AD_Median_Derived)) { range_01(z, start, stop) }
dist_scale_01_x_km <- function(x, start = min(janno_final$x), stop = max(janno_final$x)) { dist_scale_01(x * 1000, start, stop) }
dist_scale_01_y_km <- function(y, start = min(janno_final$y), stop = max(janno_final$y)) { dist_scale_01(y * 1000, start, stop) }
dist_scale_01_z_years <- function(z, start = min(janno_final$Date_BC_AD_Median_Derived), stop = max(janno_final$Date_BC_AD_Median_Derived)) { dist_scale_01(z, start, stop) }

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/area.RData")
load("data/spatial/mobility_regions.RData")

#### prep independent variables with temporal sampling ####

number_of_age_samples <- 3 #max: length(anno$calage_sample[[1]])
independent_tables <- tibble::tibble(
  independent_table = c(
    list(
      dplyr::transmute(
        .data = janno_final,
        x = range_01_x(x),
        y = range_01_y(y),
        z = range_01_z(Date_BC_AD_Median_Derived)
      )
    ), 
    lapply(
      1:number_of_age_samples, 
      function(i, anno) {
        age_sample <- sapply(janno_final$Date_BC_AD_Sample, function(x){ x[i] })
        dplyr::transmute(
          .data = janno_final,
          x = range_01_x(x),
          y = range_01_y(y),
          z = range_01_z(age_sample)
        )
      },
      anno
    )
  ),
  independent_table_id = c("age_center", paste0("age_sample_", 1:(length(independent_table) - 1)))
)

#### create spatial prediction grid ####

pred_points_space <- area %>% 
  sf::st_make_grid(cellsize = 100000, what = "centers") %>%
  sf::st_intersection(area) %>%
  sf::st_coordinates() %>%
  tibble::as_tibble() %>%
  dplyr::rename(x_real = X, y_real = Y)

time_layers <- tibble::tibble(
  age_sample = seq(-7500, -500, 100)
)

pred_grid <- pred_points_space %>% 
  tidyr::crossing(time_layers) %>%
  dplyr::mutate(
    point_id = 1:nrow(.),
    x = range_01_x(x_real),
    y = range_01_y(y_real),
    z = range_01_z(age_sample)
  )

#### create kernel parameters ####

kernel_settings <- tibble::tibble(
  kernel_setting = list(
    #ds50_dt100_g01 = list(auto = F, d = c(dist_scale_01_x_km(50), dist_scale_01_x_km(50), dist_scale_01_z_years(100)), g = 0.1),
    #ds100_dt200_g01 = list(auto = F, d = c(dist_scale_01_x_km(100), dist_scale_01_x_km(100), dist_scale_01_z_years(200)), g = 0.1),
    ds200_dt400_g01 = list(auto = F, d = c(dist_scale_01_x_km(200), dist_scale_01_x_km(200), dist_scale_01_z_years(400)), g = 0.1, on_residuals = T, auto = F)
  ),
  kernel_setting_id = names(kernel_setting)
)

#### prepare model grid ####

model_grid <- expand.grid(
  kernel_setting_id = kernel_settings$kernel_setting_id,
  dependent_var_id = c("C1", "C2"),
  independent_table_id = independent_tables$independent_table_id,
  stringsAsFactors = F
) %>%
  dplyr::left_join(
    kernel_settings, by = "kernel_setting_id"
  ) %>%
  dplyr::left_join(
    independent_tables, by = "independent_table_id"
  ) %>% dplyr::mutate(
    dependent_var = lapply(dependent_var_id, function(x) { janno_final[[x]] })
  ) %>% tibble::as_tibble()

names(model_grid$independent_table) <- model_grid$independent_table_id
names(model_grid$dependent_var) <- model_grid$dependent_var_id
model_grid$pred_grid_id <- "a"
model_grid$pred_grid <- list(pred_grid)
names(model_grid$pred_grid) <- model_grid$pred_grid_id

model_grid_result <- mobest::run_model_grid(model_grid)

#### unnest prediction to get a point-wise prediction table ####

pred_grid_filled <- mobest::unnest_model_grid(model_grid_result)
# 
# library(laGP)
# 
# #### kriging function ####
# 
# predictgp <- function(independent, dependent, pred_grid, auto = T, d, g) {
#   # priors for the global GP
#   if (auto) {
#     da <- darg(list(mle = TRUE, max=10), independent)
#     ga <- garg(list(mle = TRUE, max=10), dependent)
#     d <- da$start
#     g <- ga$start
#   }
#   # fit the global GP
#   gp <- newGPsep(X = independent, Z = dependent, d = d, g = g, dK = auto)
#   # optimise fit automatically
#   if (auto) {
#     mleGPsep(
#       gpsepi = gp,
#       param = "both",
#       tmin = c(da$min, ga$min), tmax = c(da$max, ga$max), ab = c(da$ab, ga$ab),
#       maxit = 200
#     )
#   }
#   # predictions from the global GP on the prediction
#   pred <- predGPsep(gp, XX = pred_grid[, c("x", "y", "z")], lite = T)
#   # delete GP object
#   deleteGPsep(gp)
#   # return result
#   return(pred)
# }
# 
# #### run kriging ####
# 
# prediction <- lapply(1:nrow(model_grid), function(i) {
#   predictgp(
#     model_grid[["independent_table"]][[i]],
#     model_grid[["dependent_var"]][[i]],
#     pred_grid,
#     model_grid[["kernel_setting"]][[i]][["auto"]],
#     model_grid[["kernel_setting"]][[i]][["d"]],
#     model_grid[["kernel_setting"]][[i]][["g"]]
#   )
# })
# 
# 
# #### simplified model_grid ####
# 
# model_grid_simplified <- model_grid %>%
#   dplyr::mutate(independent_table_type = ifelse(independent_table_id == "age_center", "age_center", "age_sampled")) %>%
#   dplyr::select(-kernel_setting, -independent_table, -dependent_var)
# 
# #### add prediction results for each run as a data.frame in a list column to model_grid ####
# 
# model_grid_simplified$prediction_sample <- lapply(prediction, function(x) {
#   data.frame(
#     point_id = 1:length(x$mean),
#     mean = x$mean,
#     sd = sqrt(x$s2),
#     stringsAsFactors = F
#   )
# })
# 
# #### unnest prediction to get a point-wise prediction table ####
# 
# pred_grid_filled_without_pos <- model_grid_simplified %>%
#   tidyr::unnest(cols = "prediction_sample")
# 
# #### merge with pred_grid to add other relevant, spatial information ####
# 
# pred_grid_filled <- pred_grid %>%
#   dplyr::select(-c("x", "y", "z")) %>%
#   dplyr::left_join(
#     pred_grid_filled_without_pos, by = "point_id"
#   )

###

schu <- pred_grid_filled %>% 
  dplyr::mutate(
    x = x_real,
    y = y_real, 
    z = age_sample,
  ) %>% dplyr::select(
    -x_real, -y_real, -age_sample, -pred_grid
  )

pri_ready <- mobest::search_spatial_origin(schu)

# pri <- pred_grid_filled %>%
#   tidyr::pivot_wider(names_from = "dependent_var_id", values_from = c("mean", "sd"))
# 
# # add new columns for output dataset
# pri %<>%
#   dplyr::mutate(
#     angle = NA,
#     genetic_distance = NA,
#     spatial_distance = NA,
#     mean_C1_origin = NA,
#     mean_C2_origin = NA,
#     x_real_origin = NA,
#     y_real_origin = NA,
#   )
# 
# age_sample_run_pris <- pri %>% split(list(pri$independent_table_id, pri$kernel_setting_id))
# 
# pri_ready_large <- pbapply::pblapply(age_sample_run_pris, function(age_sample_run_pri) {
#   
#   # split dataset by age slice
#   time_pris <- age_sample_run_pri %>% split(age_sample_run_pri$age_sample)
#   
#   for (p1 in 2:length(time_pris)) {
#     
#     # calculate spatial distance matrix between past and current points
#     current_pri_spatial <- as.matrix(time_pris[[p1]][c("x_real", "y_real")])
#     past_pri_spatial <- as.matrix(time_pris[[p1 - 1]][c("x_real", "y_real")])
#     spatial_distance <- fields::rdist(current_pri_spatial, past_pri_spatial)
#     
#     # calculate PCA distance matrix between past and current points
#     current_pri_genetics <- as.matrix(time_pris[[p1]][c("mean_C1", "mean_C2")])
#     past_pri_genetics <- as.matrix(time_pris[[p1 - 1]][c("mean_C1", "mean_C2")])
#     genetic_distance <- fields::rdist(current_pri_genetics, past_pri_genetics)
#     
#     # get points with least distance in the past
#     closest_point_indezes <- sapply(1:nrow(current_pri_genetics), function(x) { 
#       gendists <- genetic_distance[x,]
#       gendists[spatial_distance[x,] > 500000] <- NA
#       which.min(gendists) 
#     })
#     
#     # add closest points info to current age slice points
#     time_pris[[p1]] <- time_pris[[p1]] %>% dplyr::mutate(
#       mean_C1_origin = time_pris[[p1 - 1]]$mean_C1[closest_point_indezes],
#       mean_C2_origin = time_pris[[p1 - 1]]$mean_C2[closest_point_indezes],
#       x_real_origin = time_pris[[p1 - 1]]$x_real[closest_point_indezes],
#       y_real_origin = time_pris[[p1 - 1]]$y_real[closest_point_indezes]
#     )
#     time_pris[[p1]]$spatial_distance <- purrr::map2_dbl(
#       1:811, closest_point_indezes, function(i, j) { spatial_distance[i, j] }
#     )
#     time_pris[[p1]]$genetic_distance <- purrr::map2_dbl(
#       1:811, closest_point_indezes, function(i, j) { genetic_distance[i, j] }
#     )
#     
#     # get spatial position of entangled points
#     A <- as.matrix(time_pris[[p1]][c("x_real", "y_real")])
#     B <- as.matrix(time_pris[[p1]][c("x_real_origin", "y_real_origin")])
#     
#     # calculate angle between points
#     AB <- B - A
#     AC <- c(1, 0)
#     time_pris[[p1]]$angle <- sapply(
#       1:nrow(time_pris[[p1]]), function(i) {
#         if (time_pris[[p1]]$y_real_origin[i] < time_pris[[p1]]$y_real[i]) {
#           2*pi - matlib::angle(AB[i,], AC, degree = FALSE)
#         } else {
#           matlib::angle(AB[i,], AC, degree = FALSE)
#         }
#       }
#     )
#     
#   }
#   
#   # rowbind distance table
#   pri_ready <- time_pris[2:length(time_pris)] %>% do.call(rbind, .)
#   
#   return(pri_ready)
#   
# }, cl = 8)
# 
# pri_ready <- pri_ready_large %>% dplyr::bind_rows()
# 
# # add angle in degrees
# pi_rad <- units::as_units(pri_ready$angle, "radians")
# pi_deg <- units::set_units(pi_rad, "degrees")
# pri_ready$angle_degree <- as.numeric(pi_deg)
# 
# # spatialize
# pri_ready_spatial <- sf::st_as_sf(
#   pri_ready, 
#   coords = c("x_real", "y_real"), 
#   crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
#   remove = F
# )

###


pri_mean <- mobest::estimate_mobility(pri_ready, mobility_regions)

# points_regions <- pred_grid %>% 
#   dplyr::select(x_real, y_real, point_id) %>%
#   sf::st_as_sf( 
#     coords = c("x_real", "y_real"), 
#     crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs"
#   ) %>%
#   sf::st_intersection(mobility_regions) %>%
#   tibble::as_tibble() %>%
#   dplyr::select(-geometry)
# 
# pri <- pri_ready %>%
#   dplyr::left_join(points_regions, by = "point_id")
# 
# pri_mean <- pri %>%
#   dplyr::group_by(
#     independent_table_id, kernel_setting_id, age_sample, region_id
#   ) %>%
#   dplyr::summarise(
#     mean_km_per_decade = mean(spatial_distance)/1000/10#,
#     #mean_angle = mean(circular::circular(angle_degree, type="angles", units="degrees",modulo="2pi", template='geographics'), na.rm = T)
#   ) %>%
#   dplyr::ungroup()


####

pri_mean %>%   ggplot() +
  geom_line(
    aes(
      x = z, y = mean_km_per_decade,
      group = interaction(independent_table_id, kernel_setting_id)#,
      #color = angle_deg
    ),
    alpha = 0.5
  ) +
  facet_grid(cols = dplyr::vars(region_id), rows = dplyr::vars(kernel_setting_id))



