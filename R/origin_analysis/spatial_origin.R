library(ggplot2)

load("data/gpr/pred_grid_mean.RData")

# reduce input grid to simplifiy visualization later
pri <- pred_grid_mean%>% dplyr::group_by(
  age_sample
) %>%
  dplyr::filter(dplyr::row_number() %% 2 == 0) %>%
  dplyr::filter(dplyr::row_number() %% 2 == 0) %>%
  dplyr::ungroup()

# pri %>%
#   dplyr::select(
#     x_real, y_real
#   ) %>%
#   unique %>% plot

# add new columns for output dataset
pri %<>%
  dplyr::mutate(
    angle = NA,
    gen_distance = NA,
    spatial_distance = NA,
    pred_PC1_mean_origin = NA,
    pred_PC2_mean_origin = NA,
    x_real_origin = NA,
    y_real_origin = NA,
  )

# split dataset by age slice
time_pris <- pri %>% split(pri$age_sample)

for (p1 in 2:length(time_pris)) {
  
  # get PCA position for each spatial point in current and past age slice
  current_pri <- as.matrix(time_pris[[p1]][c("pred_PC1_mean", "pred_PC2_mean")])
  past_pri <- as.matrix(time_pris[[p1 - 1]][c("pred_PC1_mean", "pred_PC2_mean")])
  
  # calculate PCA distance matrix
  distance <- fields::rdist(current_pri, past_pri)
  
  # get points with least distance in the past
  closest_point_indezes <- sapply(1:nrow(current_pri), function(x) { which.min(distance[x,]) })
  
  # add closest points info to current age slice points
  time_pris[[p1]] <- time_pris[[p1]] %>% dplyr::mutate(
    pred_PC1_mean_origin = time_pris[[p1 - 1]]$pred_PC1_mean[closest_point_indezes],
    pred_PC2_mean_origin = time_pris[[p1 - 1]]$pred_PC2_mean[closest_point_indezes],
    x_real_origin = time_pris[[p1 - 1]]$x_real[closest_point_indezes],
    y_real_origin = time_pris[[p1 - 1]]$y_real[closest_point_indezes]
  )
  
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
  
  # store genetic distance between points
  time_pris[[p1]]$gen_distance <- sapply(
    1:nrow(time_pris[[p1]]), function(i) {
      distance[i,  which.min(distance[i,])]
    }
  )
  
  # calculate spatial distance between points
  time_pris[[p1]]$spatial_distance <- sapply(
    1:nrow(time_pris[[p1]]), function(i) {
      sqrt((time_pris[[p1]]$x_real[i] - time_pris[[p1]]$x_real_origin[i])^2 + (time_pris[[p1]]$y_real[i] - time_pris[[p1]]$y_real_origin[i])^2)
    }
  )
  
}

# rowbind distance table
pri_ready <- time_pris[2:length(time_pris)] %>% do.call(rbind, .)

