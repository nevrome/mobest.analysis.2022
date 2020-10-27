library(magrittr)
library(ggplot2)

load("data/poseidon_data/janno_final.RData")
load("data/plot_reference_data/no_data_windows.RData")
load("data/plot_reference_data/no_data_windows_yearwise.RData")

mobility <- lapply(
  list.files("data/mobility_estimation/age_resampling+origin_search+directed_speed+one_kernel_setting", full.names = T),
  function(x) {
    load(x)
    mobility_proxy
  }
) %>% dplyr::bind_rows()

# moving average
mean_mobility <- mobility %>%
  dplyr::group_by(independent_table_id, kernel_setting_id, region_id, z) %>%
  dplyr::summarise(
    mean_speed_km_per_decade = mean(speed_km_per_decade),
    sd_speed_km_per_decade = sd(speed_km_per_decade)
  ) %>%
  dplyr::filter(
    !is.na(region_id)
  )

#### mobility estimator curves ####

p_estimator <- mean_mobility %>%
  ggplot() +
  geom_line(
    aes(
      x = z, y = mean_speed_km_per_decade,
      group = independent_table_id
    ),
    size = 0.2
  ) +
  geom_point(
    data = janno_final,
    aes(x = Date_BC_AD_Median_Derived, y = 0),
    shape = "|"
  ) +
  geom_rect(
    data = no_data_windows,
    aes(
      xmin = min_date_not_covered, xmax = max_date_not_covered,
      ymin = -300, ymax = 500
    ),
    alpha = 0.3, fill = "red"
  ) +
  facet_wrap(dplyr::vars(region_id)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 40, hjust = 1)
  ) +
  xlab("time in years calBC/calAD") +
  ylab("\"Speed\" [km/decade]") +
  scale_x_continuous(breaks = seq(-7000, 1000, 1000)) +
  coord_cartesian(ylim = c(-0, max(mean_mobility$mean_speed_km_per_decade, na.rm = T))) +
  xlab("")

ggsave(
  paste0("plots/figure_sup_16_mobility_curves_mobility_curves_mean_age+origin_search+absolute_speed+multiple_kernel_settings.png"),
  plot = p_estimator,
  device = "png",
  scale = 0.5,
  dpi = 300,
  width = 700, height = 350, units = "mm",
  limitsize = F
)
