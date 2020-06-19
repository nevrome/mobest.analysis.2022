# scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/crossvalidation/interpol_comparison_* .

library(magrittr)
library(ggplot2)

interpol_comparison <- lapply(
  list.files("data/crossvalidation_2", pattern = "interpol_comparison_[0-9]", full.names = T), function(x) {
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

minicg <- interpol_comparison_group %>% 
  dplyr::group_by(dependent_var) %>%
    dplyr::filter(
    mean_squared_difference %in% (mean_squared_difference %>% sort %>% unique %>% head(20))
  ) %>% 
  dplyr::ungroup()

interpol_comparison_group %>%
  ggplot() +
  geom_raster(
    aes(x = ds, y = dt, fill = mean_squared_difference)
  ) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~dependent_var) +
  geom_raster(
    data = minicg,
    aes(x = ds, y = dt),
    fill = "red"
  ) +
  coord_fixed()

###

mean_interpol_comparison_group <- interpol_comparison_group %>% 
  dplyr::group_by(dependent_var) %>%
  dplyr::mutate(
    mean_squared_difference = (mean_squared_difference - min(mean_squared_difference))/(max(mean_squared_difference) - min(mean_squared_difference))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(kernel_setting_id, ds, dt, g) %>%
  dplyr::summarise(
    mean_mean_squared_difference = mean(mean_squared_difference)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    cut_mean_mean_squared_difference = cut(mean_mean_squared_difference, breaks = c(seq(0, 0.2, 0.05), seq(0.3, 1, 0.1)))
  )
  
mean_interpol_comparison_group %>%
  ggplot() +
  geom_raster(
    aes(x = ds, y = dt, fill = cut_mean_mean_squared_difference)
  ) +
  scale_fill_viridis_d(direction = -1) +
  coord_fixed() +
  theme_bw() +
  theme(
    legend.key.height = unit(2, "cm")
  ) +
  guides(
    fill = guide_colorsteps(title = "Mean normalized difference")
  )

###
cut_interpol_comparison <- interpol_comparison %>%
  dplyr::mutate(
    cut_difference = as.numeric(as.character(
      cut(
        difference, 
        breaks = seq(-0.1, 0.1, 0.0005), 
        labels = c(seq(-0.1, -0.0005, 0.0005), seq(0.0005, 0.1, 0.0005))
      )
    ))
  )

count_interpol_comparison <- cut_interpol_comparison %>%
  dplyr::group_by(dependent_var, cut_difference) %>%
  dplyr::summarise(
    count = dplyr::n()
  ) %>%
  dplyr::ungroup()
  
count_interpol_comparison %>%
  ggplot(aes(x = cut_difference, y = dependent_var)) +
  ggridges::geom_density_ridges(
    mapping = aes(height = count, group = dependent_var),
    scale = 2,
    stat = "identity"
  ) +
  geom_vline(
    xintercept = 0
  ) +
  theme_bw()
  
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
# theme_bw()
