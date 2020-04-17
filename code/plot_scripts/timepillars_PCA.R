library(magrittr)
library(ggplot2)

load("data/gpr/pred_grid_filled_grouped_spatial.RData")
load("data/anno_1240K_and_anno_1240K_HumanOrigins_pca.RData")
load("data/timepillars/poi.RData")
ref_pops <- readLines("data/population_lists/PCA_6.pops")
ref_pops_grouping <- readr::read_tsv("data/population_lists/contemporary_group_info_180131.txt", col_names = T)

# pca reference table
pca_ref <- anno_1240K_and_anno_1240K_HumanOrigins_pca %>%
  dplyr::filter(
    group_label %in% ref_pops
  ) %>%
  dplyr::left_join(
    ref_pops_grouping, by = c("group_label" = "Pop")
  )

# pois
toi <- lapply(
  1:nrow(poi), function(i) {
    dm <- sf::st_distance(pred_grid_filled_grouped_spatial, poi[i,])
    pred_grid_filled_grouped_spatial[which(min(dm) == dm),]
  }
)

toi_dots <- lapply(
  toi, function(t) {
    tibble::tibble(
      x = t$x_real[1],
      y = t$y_real[1]
    )
  }
) %>% dplyr::bind_rows()

names(toi) <- poi$poi_id
toi_dots$poi_id <- poi$poi_id

#### plot timelines ####

plotfun <- function(pdi, poi, iti, ksi) {
  plot <- ggplot() +
    geom_point(
      data = pca_ref,
      aes(x = PC1, y = PC2),
      color = "grey"
    ) +
    geom_path(
      data = pdi,
      aes(x = mean_PC1, y = mean_PC2)
    ) +
    geom_errorbar(
      data = pdi,
      aes(
        x = mean_PC1, 
        ymin = mean_PC2 - sd_PC2, ymax = mean_PC2 + sd_PC2,
        color = age_sample
      ),
      alpha = 0.5
    ) +
    geom_errorbarh(
      data = pdi,
      aes(
        y = mean_PC2, 
        xmin = mean_PC1 - sd_PC1, xmax = mean_PC1 + sd_PC1,
        color = age_sample
      ),
      alpha = 0.5
    ) +
    geom_point(
      data = pdi,
      aes(
        x = mean_PC1, 
        y = mean_PC2,
        color = age_sample
      ),
      size = 3
    ) +
    scale_color_gradient2(
      limits = c(-7500, -500), low = "black", mid = "red", high = "green", midpoint = -5000
    ) +
    xlim(-0.08, 0.08) +
    ylim(-0.1, 0.06) +
    theme_bw() +
    theme(
      legend.position = "bottom"
    ) +
    guides(
      color = guide_colorbar(title = "time calBC [y]", barwidth = 20)
    )
  
  plot %>% 
    ggsave(
      paste0("plots/timepillars_PCA/timepillar_", paste(c(poi, ksi, iti), collapse = "_"), ".jpeg"),
      plot = .,
      device = "jpeg",
      scale = 0.5,
      dpi = 300,
      width = 550, height = 300, units = "mm",
      limitsize = F
    )
  
}
  
plot_grid <- pred_grid_filled_grouped_spatial %>% 
  tibble::as_tibble() %>%
  dplyr::select(kernel_setting_id, independent_table_type) %>%
  unique %>%
  tidyr::crossing(
    toi_dots
  )

pred_data <- pred_grid_filled_grouped_spatial %>%
  dplyr::filter(
    age_sample %% 500 == 0
  )

lapply(1:nrow(plot_grid), function(i) {
  poi <- plot_grid$poi_id[i]
  iti <- plot_grid$independent_table_type[i]
  ksi <- plot_grid$kernel_setting_id[i] 
  pdi <- pred_data %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(
      independent_table_type == plot_grid$independent_table_type[i], 
      kernel_setting_id == plot_grid$kernel_setting_id[i],
      x_real == plot_grid$x[i],
      y_real == plot_grid$y[i]
    ) %>% dplyr::select(
      x_real, y_real, age_sample, dependent_var_id, mean, sd
    ) %>%
      tidyr::pivot_wider(names_from = "dependent_var_id", values_from = c("mean", "sd"))
  plotfun(pdi, poi, iti, ksi)
})

