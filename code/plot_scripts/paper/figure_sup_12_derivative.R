library(magrittr)
library(ggplot2)

load("data/gpr/temporal_change_median.RData")

#### derivative estimator curves ####

p_estimator <- temporal_change %>%
  ggplot() +
  geom_line(
    aes(
      x = z, y = mean_change_combined, 
      color = mean_sd_norm
    )
  ) +
  geom_line(
    aes(
      x = z, y = movavg, alpha = 1 - mean_sd_norm
    ),
    color = "blue", size = 1
  ) +
  facet_grid(cols = dplyr::vars(region_id), rows = dplyr::vars(kernel_setting_id)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 40, hjust = 1),
    strip.background = element_rect(fill = NA)
  ) +
  xlab("time calBC/calAD [y]") +
  ylab("\"Change\" [MDS-distance/50years]") +
  scale_color_gradientn(
    colours = c("red", "grey", "grey")
  ) +
  scale_x_continuous(breaks = c(-7000, -5000, -3000, -1000, 1000)) +
  scale_alpha_continuous(guide = FALSE) +
  coord_cartesian(ylim = c(0, 0.02))

ggsave(
  paste0("plots/figure_sup_12_derivative.png"),
  plot = p_estimator,
  device = "png",
  scale = 0.5,
  dpi = 300,
  width = 1000, height = 1000, units = "mm",
  limitsize = F
)

