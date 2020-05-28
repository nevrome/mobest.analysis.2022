# scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/crossvalidation/interpol_comparison_* .

library(magrittr)
library(ggplot2)

interpol_comparison <- lapply(
  list.files("data/crossvalidation", pattern = "interpol_comparison_[0-9]", full.names = T), function(x) {
    load(x)
    interpol_comparison
  }
) %>% dplyr::bind_rows()

# group difference by kernel and dependent_dist
interpol_comparison_group <- interpol_comparison %>%
  dplyr::group_by(kernel_setting_id, ds, dt, g, dependent_var) %>%
  dplyr::summarise(
    mean_squared_difference = mean(difference^2),
  ) %>%
  dplyr::ungroup()

dependent_vars <- interpol_comparison_group$dependent_var

ps <- lapply(dependent_vars %>% unique, function(cur_dependent_var) {
  
  interpol_comparison_group_dependent_var <- interpol_comparison_group %>% dplyr::filter(dependent_var == cur_dependent_var)
  
  minicg <- interpol_comparison_group_dependent_var %>% dplyr::filter(
    mean_squared_difference == min(mean_squared_difference)
  ) %>% dplyr::select(
    ds, dt, g
  )
  
  p <- interpol_comparison_group_dependent_var %>%
    ggplot() +
    geom_raster(
      aes(x = ds, y = dt, fill = mean_squared_difference)
    ) +
    scale_fill_viridis_c(direction = -1) +
    facet_grid(cols = vars(g)) +
    geom_point(
      data = minicg,
      aes(x = ds, y = dt),
      shape = 4,
      color = "red",
      size = 3
    )
  
  return(p)
  
})

cowplot::plot_grid(plotlist = ps, nrow = length(ps), labels = dependent_vars)



###

ggplot(interpol_comparison) +
  geom_histogram(
    mapping = aes(x = difference), bins = 100
  ) +
  facet_grid(rows = vars(dependent_var)) +
  # geom_vline(
  #   data = interpol_comparison_sd %>% dplyr::filter(!grepl("norm", PC)),
  #   mapping = aes(xintercept = sd_difference),
  #   color = "red"
  # ) +
  # geom_vline(
  #   mapping = aes(xintercept = 0),
  #   color = "black"
  # ) +
  # geom_vline(
  #   data = interpol_comparison_sd %>% dplyr::filter(!grepl("norm", PC)),
  #   mapping = aes(xintercept = -sd_difference),
  #   color = "red"
  # ) +
  # theme_bw() +
  xlim(-0.05, 0.05)
