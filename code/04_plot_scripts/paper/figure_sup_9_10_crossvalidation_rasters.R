library(magrittr)
library(ggplot2)

load("data/parameter_exploration/crossvalidation/interpol_comparison.RData")

interpol_comparison_with_CVdist <- interpol_comparison %>%
  dplyr::filter(g == 6 & dependent_var != "C3_dist") %>%
  tidyr::pivot_wider(
    id_cols = c("id", "mixing_iteration", "ds", "dt", "g"),
    names_from = "dependent_var",
    values_from = "difference"
  ) %>%
  dplyr::mutate(
    CVdist = sqrt(C1_dist^2 + C2_dist^2)
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("C"),
    names_to = "dependent_var",
    values_to = "difference"
  )

# group difference by kernel and dependent_dist
interpol_comparison_group <- interpol_comparison_with_CVdist %>%
  dplyr::group_by(ds, dt, g, dependent_var) %>%
  dplyr::summarise(
    mean_squared_difference = mean(difference^2),
    .groups = "drop"
  )

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
ps <- lapply(seq_along(icg), function(i) {
  icg[[i]] %>%
    ggplot() +
    geom_tile(
      aes(x = ds, y = dt, fill = mean_squared_difference),
      colour = "grey20"
    ) +
    scale_fill_viridis_c(direction = -1) +
    facet_wrap(~dependent_var) +
    coord_fixed() +
    theme_bw() +
    theme(
      legend.position = "right",
      axis.text = element_text(angle = 45, hjust = 1, size = 8)
    ) +
    guides(
      fill = guide_colorbar(title = "", barheight = 9)
    ) +
    xlab(latex2exp::TeX("$\\sqrt{\\theta_s}$")) +
    ylab(latex2exp::TeX("$\\sqrt{\\theta_t}$")) +
    scale_x_continuous(breaks = seq(0, 2000, 100)) +
    scale_y_continuous(breaks = seq(0, 2000, 100))
})

# add minimum point annotation
min_point <- icg[[3]] %>% 
  dplyr::filter(mean_squared_difference == min(mean_squared_difference)) %>%
  magrittr::extract(1,)

ps[[3]] <- ps[[3]] +
  geom_point(
    data = min_point,
    aes(x = ds, y = dt), 
    color = "red", pch = 4, size = 5
  ) +
  annotate(
    "text",
    x = min_point$ds + 500, y = min_point$dt,
    label = latex2exp::TeX(paste0("$\\sqrt{\\theta_s}$ = ", min_point$ds, " | \\sqrt{$\\theta_t}$ = ", min_point$dt)),
    parse = TRUE,
    color = "red",
    size = 4
  )

p <- cowplot::plot_grid(plotlist = ps, nrow = 2, ncol = 2)

ggsave(
  "plots/figure_sup_9_crossvalidation_rasters.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.8,
  dpi = 300,
  width = 350, height = 300, units = "mm",
  limitsize = F,
  bg = "white"
)
