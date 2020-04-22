library(magrittr)

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
run <- args[1]
dt_for_this_run <- as.numeric(args[2])
g_for_this_run <- as.numeric(args[3])

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final

anno_pca <- anno %>% dplyr::filter(!is.na(PC1))
anno_mds <- anno %>% dplyr::filter(!is.na(C1))

anno_pca_1 <- anno_pca %>% dplyr::filter(calage_center <= -5500)
anno_pca_2 <- anno_pca %>% dplyr::filter(calage_center > -5500, calage_center < -2500)
anno_pca_3 <- anno_pca %>% dplyr::filter(calage_center >= -2500)

anno_mds_1 <- anno_mds %>% dplyr::filter(calage_center <= -5500)
anno_mds_2 <- anno_mds %>% dplyr::filter(calage_center > -5500, calage_center < -2500)
anno_mds_3 <- anno_mds %>% dplyr::filter(calage_center >= -2500)

cross_one <- function(anno, dependent_var_type) {
  
  mobest::crossvalidate(
    independent = tibble::tibble(x = anno$x, y = anno$y, z = anno$calage_center),
    dependent = list(
      anno[[paste0(dependent_var_type, 1)]],
      anno[[paste0(dependent_var_type, 2)]],
      anno[[paste0(dependent_var_type, 3)]],
      anno[[paste0(dependent_var_type, 4)]]
    ) %>% stats::setNames(c(
      paste0(dependent_var_type, 1),
      paste0(dependent_var_type, 2),
      paste0(dependent_var_type, 3),
      paste0(dependent_var_type, 4)
    )),
    kernel = mobest::create_kernel_grid(
      ds = seq(50, 2000, 50)*1000, 
      dt = dt_for_this_run,#seq(50, 2050, 500),# 
      g = g_for_this_run#c(0.001, 0.01, 0.1)#
    )
  )
  
}

interpol_comparison_pca_1 <- cross_one(anno_pca_1, "PC")
interpol_comparison_pca_2 <- cross_one(anno_pca_2, "PC")
interpol_comparison_pca_3 <- cross_one(anno_pca_3, "PC")

interpol_comparison_mds_1 <- cross_one(anno_mds_1, "C")
interpol_comparison_mds_2 <- cross_one(anno_mds_2, "C")
interpol_comparison_mds_3 <- cross_one(anno_mds_3, "C")

interpol_comparison_pca_1 %<>% dplyr::mutate(setup = "1. < -5500")
interpol_comparison_pca_2 %<>% dplyr::mutate(setup = "2. -5500 - -2500")
interpol_comparison_pca_3 %<>% dplyr::mutate(setup = "3. > -2500")

interpol_comparison_mds_1 %<>% dplyr::mutate(setup = "1. < -5500")
interpol_comparison_mds_2 %<>% dplyr::mutate(setup = "2. -5500 - -2500")
interpol_comparison_mds_3 %<>% dplyr::mutate(setup = "3. > -2500")

interpol_comparison_split <- rbind(
  interpol_comparison_pca_1, interpol_comparison_pca_2, interpol_comparison_pca_3,
  interpol_comparison_mds_1, interpol_comparison_mds_2, interpol_comparison_mds_3
)

save(interpol_comparison_split, file = paste0("data/crossvalidation/interpol_comparison_split_", run, ".RData"))
