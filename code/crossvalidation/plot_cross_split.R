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
  dplyr::ungroup()

interpol_comparison_group_split %>%
  ggplot() +
  geom_raster(
    aes(x = ds, y = dt, fill = mean_squared_difference)
  ) +
  #scale_fill_viridis_c(direction = -1) +
  scale_fill_distiller(palette = "Spectral") +
  facet_grid(setup ~ dependent_var + g )

