library(magrittr)
library(ggplot2)

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
run <- args[1]
dt_for_this_run <- as.numeric(args[2])
g_for_this_run <- as.numeric(args[3])

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered
load("data/spatial/area.RData")
load("data/spatial/mobility_regions.RData")

#### compile randomly reordered versions of anno ####

anno_mixed_list <- lapply(1:1, function(i) { anno[sample(1:nrow(anno), replace = F), ] })

#### run prediction test for each of this versions ####

lapply(anno_mixed_list, function(anno_mixed) {

  #### split anno into 10 sections ####
  
  n <- 10
  nr <- nrow(anno_mixed)
  anno_10 <- split(anno_mixed, rep(1:n, times = diff(floor(seq(0, nr, length.out = n + 1)))))
  
  # 9 sections are used as a training dataset for the GP model
  anno_9_training <- lapply(
    1:n, function(i) {
      dplyr::bind_rows(anno_10[-i])
    }
  )
  
  # 1 section is used as a test dataset
  anno_9_test <- lapply(
    1:n, function(i) {
      anno_10[[i]]
    }
  )
  
  #### prepare model grid for current 9:1 comparison with different kernels ####
  
  model_grid <- lapply(
    1:n, function(i) {
      
      anno <- anno_9_training[[i]]
      
      # prep independent variables with temporal sampling
      independent_tables <- tibble::tibble(
        independent_table = list(dplyr::transmute(.data = anno, x = x, y = y, z = calage_center)),
        independent_table_id = i
      )
      
      # prep dependent vars
      dependent_vars <- tibble::tibble(
        dependent_var_id = c("PC1", "PC2", "PC3", "PC4")
      ) %>%
        dplyr::mutate(
          dependent_var = lapply(dependent_var_id, function(x) { anno[[x]] })
        )
      
      # create kernel parameters
      ks <- expand.grid(
        ds = seq(20, 1000, 20)*1000,
        dt = dt_for_this_run,#seq(20, 500, 20),
        g = g_for_this_run#c(0.001, 0.005, 0.01, 0.05, 0.1)
      )

      kernel_settings <- tibble::tibble(
        kernel_setting = lapply(
          1:nrow(ks), function(i) {
            list(d = c(ks[["ds"]][i], ks[["ds"]][i], ks[["dt"]][i]), g = ks[["g"]][i], on_residuals = T, auto = F)
          }
        ),
        kernel_setting_id = sapply(
          1:nrow(ks), function(i) {
            paste0(ks[["ds"]][i]/1000, "_", ks[["dt"]][i], "_", ks[["g"]][i])
          }
        )
      )
      
      # create spatiotemporal prediction grid
      pred_grids <- tibble::tibble(
        pred_grid = list(anno_9_test[[i]] %>% dplyr::transmute(x, y, z = calage_center, point_id = 1:nrow(.))),
        pred_grid_id = i
      )
      
      # merge info in prepare model grid
      model_grid <- mobest::create_model_grid(
        independent_tables = independent_tables, 
        dependent_vars = dependent_vars,
        kernel_settings = kernel_settings,
        pred_grids = pred_grids
      ) %>% 
        dplyr::select(-independent_table_type)
      
    }
  ) %>% dplyr::bind_rows()
   
  #### run interpolation on model grid ####
  
  model_grid_result <- mobest::run_model_grid(model_grid)
  
  #### unnest prediction to get a point-wise prediction table ####
  
  interpol_grid <- mobest::unnest_model_grid(model_grid_result)
  
  #### merge prediction and real values #### 
  
  # make wide for mean and sd PC values
  interpol_grid_wide <- interpol_grid %>% tidyr::pivot_wider(
    names_from = "dependent_var_id",
    values_from = c("mean", "sd")
  )
  
  # split by run training+test run setup to be able to merge with real test values
  interpol_grid_wide_split <- interpol_grid_wide %>% split(interpol_grid_wide$pred_grid_id)
  
  interpol_grid_merged <- lapply(
    unique(interpol_grid_wide$pred_grid_id), function(i) {
      interpol_grid_wide_split[[i]] %>%
        dplyr::left_join(
          anno_9_test[[i]] %>% dplyr::transmute(PC1, PC2, PC3, PC4, point_id = 1:nrow(.)),
          by = "point_id"
        )
    }
  ) %>% dplyr::bind_rows()
  
  return(interpol_grid_merged)
  
}) %>% dplyr::bind_rows() -> interpol_grid_merged_all


#### further data preparation ####

# calculate difference between observed and predicted PC values
interpol_grid_dist <- interpol_grid_merged_all %>% 
  dplyr::mutate(
    PC1_dist = PC1 - mean_PC1,
    PC2_dist = PC2 - mean_PC2,
    PC3_dist = PC3 - mean_PC3,
    PC4_dist = PC4 - mean_PC4
  ) %>%
  dplyr::select(
    kernel_setting_id, tidyselect::contains("_dist")
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("PC"),
    names_to = "PC",
    values_to = "difference"
  ) 

# turn kernel parameters into distinct columns again
interpol_comparison <- interpol_grid_dist %>%
  tidyr::separate(
    kernel_setting_id, 
    c("ds", "dt", "g"),
    sep = "_",
    convert = T,
    remove = F
  ) 

# group difference by kernel and PC (grtouping with different mechanisms like mean/meadian/sd)
interpol_comparison_group <- interpol_comparison %>%
  dplyr::group_by(kernel_setting_id, ds, dt, g, PC) %>%
  dplyr::summarise(
    mean_difference = mean(abs(difference)),
    median_difference = median(abs(difference)),
    sd_difference = sd(difference),
    diff_5_95_difference = diff(quantile(difference, probs = c(0.05, 0.95)))
  ) %>%
  dplyr::ungroup()

save(interpol_comparison_group, file = paste0("data/crossvalidation/interpol_comparison_group_", run, ".RData"))
