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

#### pca ####

interpol_grid_pca <- mobest::crossvalidate(
  independent = tibble::tibble(x = anno$x, y = anno$y, z = anno$calage_center),
  dependent = list(
    PC1 = anno$PC1,
    PC2 = anno$PC2,
    PC3 = anno$PC3,
    PC4 = anno$PC4
  ),
  kernel = mobest::create_kernel_grid(
    ds = seq(50, 2050, 1000)*1000, 
    dt = dt_for_this_run, 
    g = g_for_this_run
  )
)

interpol_comparison_pca <- interpol_grid_pca %>%
  dplyr::mutate(
    PC1_dist = PC1 - mean_PC1,
    PC2_dist = PC2 - mean_PC2,
    PC3_dist = PC3 - mean_PC3,
    PC4_dist = PC4 - mean_PC4
  ) %>%
  dplyr::select(
    kernel_setting_id, ds, dt, g, tidyselect::contains("_dist")
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("PC"),
    names_to = "PC",
    values_to = "difference"
  )

#### 

save(interpol_comparison, file = paste0("data/crossvalidation/interpol_comparison_", run, ".RData"))
