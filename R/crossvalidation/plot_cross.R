load("data/crossvalidation/interpol_comparison_group.RData")




library(magrittr)
library(ggplot2)

interpol_comparison_group %>%
  ggplot() +
  geom_raster(
    aes(x = ds, y = dt, fill = sd_difference)
  ) +
  scale_fill_viridis_c() +
  facet_grid(rows = vars(PC), cols = vars(g))

  
  