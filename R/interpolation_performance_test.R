library(magrittr)

#### data ####

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered
load("data/spatial/area.RData")
load("data/spatial/mobility_regions.RData")

#### split ####

anno_mixed <- anno[sample(1:nrow(anno), replace = F), ]

n <- 10
nr <- nrow(anno_mixed)
anno_10 <- split(anno_mixed, rep(1:n, times = diff(floor(seq(0, nr, length.out = n + 1)))))

anno_9_training <- lapply(
  1:n, function(i) {
    dplyr::bind_rows(anno_10[-i])
  }
)

anno_9_test <- lapply(
  1:n, function(i) {
    anno_10[[i]]
  }
)

#### prepare model grid ####

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
    kernel_settings <- tibble::tibble(
      kernel_setting = list(
        ds50_dt100_g01 = list(auto = F, d = c(50000, 50000, 100), g = 0.1),
        ds100_dt200_g01 = list(auto = F, d = c(100000, 100000, 200), g = 0.1),
        ds200_dt400_g01 = list(auto = F, d = c(200000, 200000, 400), g = 0.1)
      ),
      kernel_setting_id = names(kernel_setting)
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
    )
    
  }
) %>% dplyr::bind_rows()
 
#### run interpolation on model grid ####

model_grid_result <- mobest::run_model_grid(model_grid)

#### unnest prediction to get a point-wise prediction table ####

interpol_grid <- mobest::unnest_model_grid(model_grid_result)

#### compare prediction and real values #### 

hu <- interpol_grid %>% tidyr::pivot_wider(
  names_from = "dependent_var_id",
  values_from = c("mean", "sd")
)

spu <- hu %>% split(hu$pred_grid_id)

gnu <- lapply(
  unique(hu$pred_grid_id), function(i) {
    spu[[i]] %>%
      dplyr::left_join(
        anno_9_test[[i]] %>% dplyr::transmute(PC1, PC2, PC3, PC4, point_id = 1:nrow(.)),
        by = "point_id"
      )
  }
) %>% dplyr::bind_rows()

plu <- gnu %>% 
  dplyr::mutate(
    PC1_dist = PC1 - mean_PC1,
    PC2_dist = PC2 - mean_PC2,
    PC3_dist = PC3 - mean_PC3,
    PC4_dist = PC4 - mean_PC4,
    PC1_dist_norm = PC1_dist / diff(range(PC1)),
    PC2_dist_norm = PC2_dist / diff(range(PC2)),
    PC3_dist_norm = PC3_dist / diff(range(PC3)),
    PC4_dist_norm = PC4_dist / diff(range(PC4))
  ) %>%
  dplyr::select(
    kernel_setting_id, tidyselect::contains("_dist")
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("PC"),
    names_to = "PC",
    values_to = "difference"
  )

library(ggplot2)

plu %>%
  dplyr::filter(!grepl("norm", PC)) %>%
  ggplot() +
  geom_histogram(
    aes(x = difference, fill = kernel_setting_id), bins = 30
  ) +
  facet_grid(rows = vars(PC), cols = vars(kernel_setting_id)) +
  geom_vline(aes(xintercept = 0)) +
  theme_bw() +
  xlab("")

plu %>%
  dplyr::filter(grepl("norm", PC)) %>%
  ggplot() +
  geom_histogram(
    aes(x = difference, fill = kernel_setting_id), bins = 30
  ) +
  facet_grid(rows = vars(PC), cols = vars(kernel_setting_id)) +
  geom_vline(aes(xintercept = 0)) +
  theme_bw()
