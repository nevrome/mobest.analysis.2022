library(magrittr)
library(laGP)

load("/projects1/coest_mobility/coest.interpol.2020/data/gpr/gpr_temporal_sampling.RData")

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

save(pred_grid, file = "/projects1/coest_mobility/coest.interpol.2020/data/gpr/pred_grid_temporal_sampling.RData")

