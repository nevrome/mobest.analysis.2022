# scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/crossvalidation/interpol_comparison_split_* .

library(magrittr)
library(ggplot2)

interpol_comparison_split <- lapply(
  list.files("data/crossvalidation", pattern = "interpol_comparison_split_[0-9]*", full.names = T), function(x) {
    load(x)
    interpol_comparison_split
  }
) %>% dplyr::bind_rows()

interpol_comparison_group_split <- interpol_comparison_split %>% dplyr::filter(
    dependent_var %in% c("PC1_dist", "C1_dist")
  ) %>%
  dplyr::group_by(kernel_setting_id, ds, dt, g, dependent_var, setup) %>%
  dplyr::summarise(
    mean_squared_difference = mean(difference^2),
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(dependent_var, setup, g) %>%
  dplyr::mutate(
    min = mean_squared_difference == min(mean_squared_difference),
    #mean_squared_difference = mean_squared_difference - min(mean_squared_difference)
  ) %>%
  dplyr::ungroup()


ggplot() +
  geom_raster(
    data = interpol_comparison_group_split,
    mapping = aes(x = ds, y = dt, fill = mean_squared_difference)
  ) +
  geom_point(
    data = interpol_comparison_group_split %>% dplyr::filter(min),
    mapping = aes(x = ds, y = dt),
    shape = 4,
    color = "red",
    size = 3
  ) +
  #scale_fill_viridis_c(direction = -1) +
  scale_fill_distiller(palette = "Spectral") +
  facet_grid(setup ~ dependent_var + g )

