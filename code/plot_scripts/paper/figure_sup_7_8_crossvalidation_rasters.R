library(magrittr)
library(ggplot2)

load("data/parameter_exploration/crossvalidation/interpol_comparison.RData")

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

icg <- interpol_comparison_group %>% dplyr::group_split(dependent_var)
mg <- minicg %>% dplyr::group_split(dependent_var)

# for each ancestry component
ps1 <- lapply(1:2, function(i) {
  icg[[i]] %>%
    ggplot() +
    geom_raster(
      aes(x = ds, y = dt, fill = mean_squared_difference)
    ) +
    scale_fill_viridis_c(direction = -1) +
    facet_wrap(~dependent_var) +
    geom_raster(
      data = mg[[i]],
      aes(x = ds, y = dt),
      fill = "red"
    ) +
    coord_fixed() +
    theme(
      legend.position = "right"
    ) +
    theme_bw() +
    guides(
      fill = guide_colorbar(title = "", barheight = 9)
    ) +
    xlab(latex2exp::TeX("$\\sqrt{\\theta_s}$")) +
    ylab(latex2exp::TeX("$\\sqrt{\\theta_t}$"))
})

p1 <- cowplot::plot_grid(plotlist = ps1, nrow = 1, ncol = 2)

ggsave(
  "plots/figure_sup_7_crossvalidation_rasters.jpeg",
  plot = p1,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 350, height = 130, units = "mm",
  limitsize = F
)

# merge ancestry components
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
    cut_mean_mean_squared_difference = cut(
      mean_mean_squared_difference, 
      breaks = c(seq(0, 0.2, 0.05), seq(0.3, 1, 0.1)),
      include.lowest = T
    )
  )

# get values
bests <- mean_interpol_comparison_group %>% dplyr::arrange(mean_mean_squared_difference) %>% head(20)
min(bests$ds) %>% sqrt()
max(bests$ds) %>% sqrt()
min(bests$dt) %>% sqrt()
max(bests$dt) %>% sqrt()

f <- Vectorize(function(x) {
  if (x < 1) return(x/1e10)
  sqrt(x)
})

min_point <- mean_interpol_comparison_group %>% 
  dplyr::filter(mean_mean_squared_difference == min(mean_mean_squared_difference)) %>%
  magrittr::extract(1,)

p2 <- mean_interpol_comparison_group %>%
  ggplot() +
  geom_raster(
    aes(x = ds, y = dt, fill = cut_mean_mean_squared_difference)
  ) +
  geom_point(
    aes(x = 600, y = 2600), 
    color = "blue", pch = 4, size = 5
  ) +
  geom_point(
    data = min_point,
    aes(x = ds, y = dt), 
    color = "red", pch = 4, size = 5
  ) +
  geom_point(
    aes(x = 1300, y = 1000), 
    color = "black", pch = 4, size = 5
  ) +
  annotate(
    "text",
    x = 600 + 3000, y = 2600, 
    label = latex2exp::TeX(paste0("$\\theta_s$ = ", 600, " | $\\theta_t$ = ", 2600)), 
    parse = TRUE, 
    color = "blue",
    size = 4
  ) +
  annotate(
    "text",
    x = min_point$ds + 3000, y = min_point$dt, 
    label = latex2exp::TeX(paste0("$\\theta_s$ = ", min_point$ds, " | $\\theta_t$ = ", min_point$dt)), 
    parse = TRUE, 
    color = "red",
    size = 4
  ) +
  annotate(
    "text",
    x = 1300 + 3000, y = 1000, 
    label = latex2exp::TeX(paste0("$\\theta_s$ = ", 1300, " | $\\theta_t$ = ", 1000)), 
    parse = TRUE, 
    color = "black",
    size = 4
  ) +
  scale_fill_viridis_d(direction = -1) +
  coord_fixed() +
  theme_bw() +
  theme(
    legend.key.height = unit(2, "cm")
  ) +
  guides(
    fill = guide_colorsteps(title = "Mean normalized difference")
  ) +
  xlab(latex2exp::TeX("$\\sqrt{\\theta_s}$")) +
  ylab(latex2exp::TeX("$\\sqrt{\\theta_t}$")) +
  scale_x_continuous(breaks = seq(0, 10000, 1000)) +
  scale_y_continuous(breaks = seq(0, 10000, 1000))
  # +
  # scale_y_continuous(sec.axis = sec_axis(~f(.), name = latex2exp::TeX("$\\sqrt{\\theta_t}$"))) +
  # scale_x_continuous(sec.axis = sec_axis(~f(.), name = latex2exp::TeX("$\\sqrt{\\theta_x}$")))

ggsave(
  "plots/figure_sup_8_crossvalidation_raster_merged.jpeg",
  plot = p2,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 200, units = "mm",
  limitsize = F
)

