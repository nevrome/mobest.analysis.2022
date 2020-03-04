library(magrittr)
library(ggplot2)

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
      limits = c(-7500, -500), low = "red", mid = "green", high = "blue", midpoint = -3000
    ) +
    theme_bw()
  
  plot %>% 
    ggsave(
      paste0("plots/individual_timelines/timeline_", paste(c(poi, ksi, iti), collapse = "_"), ".jpeg"),
      plot = .,
      device = "jpeg",
      scale = 1,
      dpi = 300,
      width = 550, height = 280, units = "mm",
      limitsize = F
    )
  
}
  
plot_grid <- pred_grid_spatial %>% 
  tibble::as_tibble() %>%
  dplyr::select(kernel_setting_id, independent_table_id) %>%
  unique %>%
  tidyr::crossing(
    toi_dots
  )

pred_data <- pred_grid_spatial %>%
  dplyr::filter(
    age_sample %% 500 == 0
  )

lapply(1:nrow(plot_grid), function(i) {
  poi <- plot_grid$poi_id[i]
  iti <- plot_grid$independent_table_id[i]
  ksi <- plot_grid$kernel_setting_id[i] 
  pdi <- pred_data %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(
      independent_table_id == plot_grid$independent_table_id[i], 
      kernel_setting_id == plot_grid$kernel_setting_id[i],
      x_real == plot_grid$x[i],
      y_real == plot_grid$y[i]
    ) %>% dplyr::select(
      x_real, y_real, age_sample, dependent_var_id, mean, sd
    ) %>%
      tidyr::pivot_wider(names_from = "dependent_var_id", values_from = c("mean", "sd"))
  plotfun(pdi, poi, iti, ksi)
})

