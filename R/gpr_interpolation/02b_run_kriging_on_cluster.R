library(magrittr)
library(laGP)

load("/projects1/coest_mobility/coest.interpol.2020/data/gpr/gpr_prep_temporal_sampling.RData")

#### kriging function ####

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

#### run kriging ####
# for every PC
prediction_list <- lapply(c("PC1", "PC2", "PC3", "PC4"), function(x, anno, pred_grid) {
  # for every time sampling run
  lapply(independent_list, function(y, x) {
      predictgp(y, anno[[x]], pred_grid)
  }, x)
}, anno, pred_grid)

#### sample from kriging result for each point ####
# for every PC
prediction_sample_list <- lapply(prediction_list, function(x) {
  # for every time sampling run
  lapply(x, function(y) {
      pred_sample = sapply(1:length(y$mean), function(i) { rnorm(1, y$mean[i], sqrt(y$s2[i])) })
  })
})

#### transform to long data.frame ####
# for every PC
prediction_sample_df <- lapply(1:length(prediction_sample_list), function(j, prediction_sample_list) {
  x <- prediction_sample_list[[j]]
  # for every time sampling run
  lapply(1:length(x), function(i, x, j) {
    data.frame(
      PC = j,
      run_id = i,
      point_id = 1:length(x[[i]]),
      pred_samples = x[[i]]
    )
  }, x, j) %>% dplyr::bind_rows()
}, prediction_sample_list) %>% dplyr::bind_rows() %>% tibble::as_tibble()

#### combine prediction for each PC and each run (mean) ####

prediction_per_point_df <- prediction_sample_df %>%
  dplyr::group_by(PC, point_id) %>%
  dplyr::summarize(mean = mean(pred_samples), sd = sd(pred_samples))

#### add prediction to pred_grid ####

pred_grid <- pred_grid %>%
  dplyr::mutate(
    pred_PC1_mean = prediction_per_point_df %>% dplyr::filter(PC == 1) %$% mean,
    pred_PC1_sd = prediction_per_point_df %>% dplyr::filter(PC == 1) %$% sd,
    pred_PC2_mean = prediction_per_point_df %>% dplyr::filter(PC == 2) %$% mean,
    pred_PC2_sd = prediction_per_point_df %>% dplyr::filter(PC == 2) %$% sd,
    pred_PC3_mean = prediction_per_point_df %>% dplyr::filter(PC == 3) %$% mean,
    pred_PC3_sd = prediction_per_point_df %>% dplyr::filter(PC == 3) %$% sd,
    pred_PC4_mean = prediction_per_point_df %>% dplyr::filter(PC == 4) %$% mean,
    pred_PC4_sd = prediction_per_point_df %>% dplyr::filter(PC == 4) %$% sd
  )

save(pred_grid, file = "/projects1/coest_mobility/coest.interpol.2020/data/gpr/pred_grid_temporal_sampling.RData")
