library(magrittr)
library(ggplot2)

mobility <- lapply(
  list.files("data/mobility_estimation_main_run", full.names = T),
  function(x) {
    load(x)
    mobility_proxy
  }
) %>% dplyr::bind_rows()

mobility$kernel_setting_id <- factor(mobility$kernel_setting_id, levels = c(
  "ds100_dt100_g001",
  "ds200_dt200_g001",
  "ds500_dt500_g001",
  "ds1000_dt1000_g001",
  "ds2000_dt2000_g001"
))


mobility %>%
  # dplyr::mutate(
  #   kernel_setting_id = dplyr::recode(
  #     kernel_setting_id, "ds100_dt200_g01" = "small kernel", "ds200_dt400_g01" = "big kernel"
  #   )
  # ) %>%
  ggplot() +
  geom_line(
    aes(
      x = z, y = mean_km_per_decade, 
      group = interaction(independent_table_id, kernel_setting_id), 
      color = kernel_setting_id
      #color = angle_deg
    ),
    alpha = 0.3
  ) +
  facet_grid(cols = dplyr::vars(region_id), rows = dplyr::vars(kernel_setting_id)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  xlab("time calBC [y]") +
  ylab("\"Speed\" [km/decade]") +
  # guides(
  #   color = guide_legend(title = "kernels", override.aes = list(size = 10, alpha = 1))
  # ) +
  # scale_color_manual(
  #   values = c(
  #     "small kernel" = "orange",
  #     "big kernel" = "darkgreen"
  #   )
  # ) +
  # scale_fill_gradient2(
  #   low = "blue",
#   mid = "red",
#   high = "blue",
#   midpoint = 180
# ) +
# scale_color_gradientn(colours = c("blue", "red", "yellow", "green", "blue")) +
  NULL
