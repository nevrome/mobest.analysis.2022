library(magrittr)
library(ggplot2)

load("data/gpr/temporal_change_median.RData")
load("data/poseidon_data/janno_final.RData")

#### derivative estimator curves ####

p_estimator <- temporal_change %>%
  dplyr::filter(!is.na(region_id)) %>%
  ggplot() +
  geom_line(aes(x = z, y = mean_change_combined), color = "red") +
  geom_point(
    data = janno_final,
    aes(x = Date_BC_AD_Median_Derived, y = 0),
    shape = "|"
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
  coord_cartesian(ylim = c(0, 0.01))

ggsave(
  paste0("plots/figure_sup_12_derivative.jpeg"),
  plot = p_estimator,
  device = "jpeg",
  scale = 0.3,
  dpi = 300,
  width = 1300, height = 600, units = "mm",
  limitsize = F
)

