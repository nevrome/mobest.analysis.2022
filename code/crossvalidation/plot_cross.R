# scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/crossvalidation/interpol_comparison_* .

library(magrittr)
library(ggplot2)

interpol_comparison <- lapply(
  list.files("data/crossvalidation", pattern = "interpol_comparison_[0-9]", full.names = T), function(x) {
    load(x)
    interpol_comparison
  }
) %>% dplyr::bind_rows()

# group difference by kernel and PC
interpol_comparison_group <- interpol_comparison %>%
  dplyr::group_by(kernel_setting_id, ds, dt, g, PC) %>%
  dplyr::summarise(
    mean_squared_difference = mean(difference^2),
  ) %>%
  dplyr::ungroup()

PCs <- interpol_comparison_group$PC

ps <- lapply(PCs %>% unique, function(cur_PC) {
  
  interpol_comparison_group_PC <- interpol_comparison_group %>% dplyr::filter(PC == cur_PC)
  
  minicg <- interpol_comparison_group_PC %>% dplyr::filter(
    mean_squared_difference == min(mean_squared_difference)
  ) %>% dplyr::select(
    ds, dt, g
  )
  
  p <- interpol_comparison_group_PC %>%
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

cowplot::plot_grid(plotlist = ps, nrow = length(ps), labels = PCs)



###

# ggplot() +
#   geom_histogram(
#     data = interpol_comparison %>% dplyr::filter(!grepl("norm", PC)),
#     mapping = aes(x = difference, fill = kernel_setting_id), bins = 100
#   ) +
#   facet_grid(cols = vars(PC), rows = vars(kernel_setting_id)) +
#   geom_vline(
#     data = interpol_comparison_sd %>% dplyr::filter(!grepl("norm", PC)), 
#     mapping = aes(xintercept = sd_difference),
#     color = "red"
#   ) +
#   geom_vline(
#     data = interpol_comparison_sd %>% dplyr::filter(!grepl("norm", PC)), 
#     mapping = aes(xintercept = 0),
#     color = "black"
#   ) +
#   geom_vline(
#     data = interpol_comparison_sd %>% dplyr::filter(!grepl("norm", PC)), 
#     mapping = aes(xintercept = -sd_difference),
#     color = "red"
#   ) +
#   theme_bw() +
#   xlim(-0.05, 0.05)
# 
# interpol_comparison %>%
#   dplyr::filter(grepl("norm", PC)) %>%
#   ggplot() +
#   geom_histogram(
#     aes(x = difference, fill = kernel_setting_id), bins = 100
#   ) +
#   facet_grid(cols = vars(PC), rows = vars(kernel_setting_id)) +
#   geom_vline(aes(xintercept = 0)) +
#   theme_bw()


  
  
  