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


interpol_grid_merged_all <- mobest::crossvalidate(
  independent = tibble::tibble(x = anno$x, y = anno$y, z = anno$calage_center),
  dependent = list(
    PC1 = anno$PC1,
    PC2 = anno$PC2,
    PC3 = anno$PC3,
    PC4 = anno$PC4
  ),
  kernel = mobest::create_kernel_grid(
    ds = seq(50, 2050, 200)*1000, 
    dt = dt_for_this_run, 
    g = g_for_this_run
  )
)

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




# #### compile randomly reordered versions of anno ####
# 
# anno_mixed_list <- lapply(1:2, function(i) { anno[sample(1:nrow(anno), replace = F), ] })
# 
# #### run prediction test for each of this versions ####
# 
# lapply(anno_mixed_list, function(anno_mixed) {
# 
#   #### split anno into 10 sections ####
#   
#   n <- 10
#   nr <- nrow(anno_mixed)
#   anno_10 <- split(anno_mixed, rep(1:n, times = diff(floor(seq(0, nr, length.out = n + 1)))))
#   
#   # 9 sections are used as a training dataset for the GP model
#   anno_9_training <- lapply(
#     1:n, function(i) {
#       dplyr::bind_rows(anno_10[-i])
#     }
#   )
#   
#   # 1 section is used as a test dataset
#   anno_9_test <- lapply(
#     1:n, function(i) {
#       anno_10[[i]]
#     }
#   )
#   
#   #### prepare model grid for current 9:1 comparison with different kernels ####
#   
#   model_grid <- lapply(
#     1:n, function(i) {
#       
#       anno <- anno_9_training[[i]]
#       
#       # create model grid
#       model_grid <- mobest::create_model_grid(
#         independent = c(
#           list(age_center = tibble::tibble(x = anno$x, y = anno$y, z = anno$calage_center))
#         ),
#         dependent = list(
#           PC1 = anno$PC1,
#           PC2 = anno$PC2,
#           PC3 = anno$PC3,
#           PC4 = anno$PC4
#         ),
#         kernel = mobest::create_kernel_grid(
#           ds = seq(50, 2050, 200)*1000, 
#           dt = dt_for_this_run, 
#           g = g_for_this_run
#         ),
#         prediction_grid = list(
#           anno_9_test[[i]] %>% dplyr::transmute(x, y, z = calage_center, point_id = 1:nrow(.))
#         ) %>% setNames(i)
#       ) %>% 
#         dplyr::select(-independent_table_type)
#       
#     }
#   ) %>% dplyr::bind_rows()
#    
#   #### run interpolation on model grid ####
#   
#   model_grid_result <- mobest::run_model_grid(model_grid)
#   
#   #### unnest prediction to get a point-wise prediction table ####
#   
#   interpol_grid <- mobest::unnest_model_grid(model_grid_result)
#   
#   #### merge prediction and real values #### 
#   
#   # make wide for mean and sd PC values
#   interpol_grid_wide <- interpol_grid %>% tidyr::pivot_wider(
#     names_from = "dependent_var_id",
#     values_from = c("mean", "sd")
#   )
#   
#   # split by run training+test run setup to be able to merge with real test values
#   interpol_grid_wide_split <- interpol_grid_wide %>% split(interpol_grid_wide$pred_grid_id)
#   
#   interpol_grid_merged <- lapply(
#     as.numeric(unique(interpol_grid_wide$pred_grid_id)), function(i) {
#       interpol_grid_wide_split[[i]] %>%
#         dplyr::left_join(
#           anno_9_test[[i]] %>% dplyr::transmute(PC1, PC2, PC3, PC4, point_id = 1:nrow(.)),
#           by = "point_id"
#         )
#     }
#   ) %>% dplyr::bind_rows()
#   
#   return(interpol_grid_merged)
#   
# }) %>% dplyr::bind_rows() -> interpol_grid_merged_all
# 
# 
# #### further data preparation ####
# 
# # calculate difference between observed and predicted PC values
# interpol_grid_dist <- interpol_grid_merged_all %>% 
#   dplyr::mutate(
#     PC1_dist = PC1 - mean_PC1,
#     PC2_dist = PC2 - mean_PC2,
#     PC3_dist = PC3 - mean_PC3,
#     PC4_dist = PC4 - mean_PC4
#   ) %>%
#   dplyr::select(
#     kernel_setting_id, tidyselect::contains("_dist")
#   ) %>%
#   tidyr::pivot_longer(
#     cols = tidyselect::starts_with("PC"),
#     names_to = "PC",
#     values_to = "difference"
#   ) 
# 
# # turn kernel parameters into distinct columns again
# interpol_comparison <- interpol_grid_dist %>%
#   tidyr::separate(
#     kernel_setting_id, 
#     c("ds", "dt", "g"),
#     sep = "_",
#     convert = T,
#     remove = F
#   ) 

save(interpol_comparison, file = paste0("data/crossvalidation/interpol_comparison_", run, ".RData"))
