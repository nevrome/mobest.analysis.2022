library(ggplot2)

load("data/gpr/pred_grid_spatial_cropped.RData")

pri <- pred_grid_spatial_cropped %>% tibble::as_tibble() %>% dplyr::select(-geometry)

pri$angle <- NA
pri$gen_distance <- NA
pri$spatial_distance <- NA
pri$pred_PC1_mean_origin <- NA
pri$pred_PC2_mean_origin <- NA
pri$x_real_origin <- NA
pri$y_real_origin <- NA
time_pris <- pri %>% split(pri$age_sample)

for (p1 in 2:length(time_pris)) {
  
  current_pri <- as.matrix(time_pris[[p1]][c("pred_PC1_mean", "pred_PC2_mean")])
  past_pri <- as.matrix(time_pris[[p1 - 1]][c("pred_PC1_mean", "pred_PC2_mean")])
  
  distance <- fields::rdist(current_pri, past_pri)
  
  closest_point_indezes <- sapply(1:nrow(current_pri), function(x) { which.min(distance[x,]) })
  time_pris[[p1]] <- time_pris[[p1]] %>% dplyr::mutate(
    pred_PC1_mean_origin = time_pris[[p1 - 1]]$pred_PC1_mean[closest_point_indezes],
    pred_PC2_mean_origin = time_pris[[p1 - 1]]$pred_PC2_mean[closest_point_indezes],
    x_real_origin = time_pris[[p1 - 1]]$x_real[closest_point_indezes],
    y_real_origin = time_pris[[p1 - 1]]$y_real[closest_point_indezes]
  )
  
  A <- time_pris[[p1]][c("x_real", "y_real")]
  A <- as.matrix(A)
  B <- time_pris[[p1]][c("x_real_origin", "y_real_origin")]
  B <- as.matrix(B)
  
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
  
  time_pris[[p1]]$gen_distance <- sapply(
    1:nrow(time_pris[[p1]]), function(i) {
      distance[i,  which.min(distance[i,])]
    }
  )
  
  time_pris[[p1]]$spatial_distance <- sapply(
    1:nrow(time_pris[[p1]]), function(i) {
      sqrt((time_pris[[p1]]$x_real[i] - time_pris[[p1]]$x_real_origin[i])^2 + (time_pris[[p1]]$y_real[i] - time_pris[[p1]]$y_real_origin[i])^2)
    }
  )
  
}
pri_ready <- time_pris[2:length(time_pris)] %>% do.call(rbind, .)

