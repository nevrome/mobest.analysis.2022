library(magrittr)

load("data/gpr/pred_grid_filled.RData")
pri <- pred_grid_filled %>%
  tidyr::pivot_wider(names_from = "dependent_var_id", values_from = c("mean", "sd"))

# add new columns for output dataset
pri %<>%
  dplyr::mutate(
    angle = NA,
    genetic_distance = NA,
    spatial_distance = NA,
    mean_PC1_origin = NA,
    mean_PC2_origin = NA,
    mean_PC3_origin = NA,
    mean_PC4_origin = NA,
    x_real_origin = NA,
    y_real_origin = NA,
  )

age_sample_run_pris <- pri %>% split(list(pri$independent_table_id, pri$kernel_setting_id))

pri_ready_large <- pbapply::pblapply(age_sample_run_pris, function(age_sample_run_pri) {

  # split dataset by age slice
  time_pris <- age_sample_run_pri %>% split(age_sample_run_pri$age_sample)
  
  for (p1 in 2:length(time_pris)) {
    
    # calculate spatial distance matrix between past and current points
    current_pri_spatial <- as.matrix(time_pris[[p1]][c("x_real", "y_real")])
    past_pri_spatial <- as.matrix(time_pris[[p1 - 1]][c("x_real", "y_real")])
    spatial_distance <- fields::rdist(current_pri_spatial, past_pri_spatial)
    
    # calculate PCA distance matrix between past and current points
    current_pri_genetics <- as.matrix(time_pris[[p1]][c("mean_PC1", "mean_PC2", "mean_PC3", "mean_PC4")])
    past_pri_genetics <- as.matrix(time_pris[[p1 - 1]][c("mean_PC1", "mean_PC2", "mean_PC3", "mean_PC4")])
    genetic_distance <- fields::rdist(current_pri_genetics, past_pri_genetics)
    
    # get points with least distance in the past
    closest_point_indezes <- sapply(1:nrow(current_pri_genetics), function(x) { 
      gendists <- genetic_distance[x,]
      gendists[spatial_distance[x,] > 500000] <- NA
      which.min(gendists) 
    })
    
    # add closest points info to current age slice points
    time_pris[[p1]] <- time_pris[[p1]] %>% dplyr::mutate(
      mean_PC1_origin = time_pris[[p1 - 1]]$mean_PC1[closest_point_indezes],
      mean_PC2_origin = time_pris[[p1 - 1]]$mean_PC2[closest_point_indezes],
      mean_PC3_origin = time_pris[[p1 - 1]]$mean_PC3[closest_point_indezes],
      mean_PC4_origin = time_pris[[p1 - 1]]$mean_PC4[closest_point_indezes],
      x_real_origin = time_pris[[p1 - 1]]$x_real[closest_point_indezes],
      y_real_origin = time_pris[[p1 - 1]]$y_real[closest_point_indezes]
    )
    time_pris[[p1]]$spatial_distance <- purrr::map2_dbl(1:817, closest_point_indezes, function(i, j) { spatial_distance[i, j] })
    time_pris[[p1]]$genetic_distance <- purrr::map2_dbl(1:817, closest_point_indezes, function(i, j) { genetic_distance[i, j] })
    
    # get spatial position of entangled points
    A <- as.matrix(time_pris[[p1]][c("x_real", "y_real")])
    B <- as.matrix(time_pris[[p1]][c("x_real_origin", "y_real_origin")])
    
    # calculate angle between points
    AB <- B - A
    AC <- c(1, 0)
    time_pris[[p1]]$angle <- sapply(
      1:nrow(time_pris[[p1]]), function(i) {
        if (time_pris[[p1]]$y_real_origin[i] < time_pris[[p1]]$y_real[i]) {
          2*pi - matlib::angle(AB[i,], AC, degree = FALSE)
        } else {
          matlib::angle(AB[i,], AC, degree = FALSE)
        }
      }
    )
    
  }

  # rowbind distance table
  pri_ready <- time_pris[2:length(time_pris)] %>% do.call(rbind, .)
  
  return(pri_ready)
  
}, cl = 8)

pri_ready <- pri_ready_large %>% dplyr::bind_rows()

# add angle in degrees
pi_rad <- units::as_units(pri_ready$angle, "radians")
pi_deg <- units::set_units(pi_rad, "degrees")
pri_ready$angle_degree <- as.numeric(pi_deg)

save(pri_ready, file = "data/pri_ready.RData")

# spatialize
pri_ready_spatial <- sf::st_as_sf(
  pri_ready, 
  coords = c("x_real", "y_real"), 
  crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs",
  remove = F
)

save(pri_ready_spatial, file = "data/pri_ready_spatial.RData")

