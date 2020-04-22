library(magrittr)

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
run <- args[1]
dt_for_this_run <- as.numeric(args[2])
g_for_this_run <- as.numeric(args[3])

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_final.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_final

#### pca ####

interpol_comparison_pca <- mobest::crossvalidate(
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

#### mds ####

anno_mds <- anno %>% dplyr::filter(
  !is.na(C1)
)

interpol_comparison_mds <- mobest::crossvalidate(
  independent = tibble::tibble(x = anno_mds$x, y = anno_mds$y, z = anno_mds$calage_center),
  dependent = list(
    C1 = anno_mds$C1,
    C2 = anno_mds$C2,
    C3 = anno_mds$C3,
    C4 = anno_mds$C4
  ),
  kernel = mobest::create_kernel_grid(
    ds = seq(50, 2050, 1000)*1000, 
    dt = dt_for_this_run, 
    g = g_for_this_run
  )
)

#### merge pca and mds ####

interpol_comparison <- rbind(interpol_comparison_pca, interpol_comparison_mds)


save(interpol_comparison, file = paste0("data/crossvalidation/interpol_comparison_", run, ".RData"))
