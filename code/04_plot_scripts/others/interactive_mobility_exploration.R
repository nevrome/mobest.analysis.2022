library(magrittr)
library(ggplot2)

#### data ####

# origin search
load("data/poseidon_data/janno_final.RData")
load("data/origin_search/origin_grid_mean.RData")

origin_grid_mean_infodense <- origin_grid_mean %>%
  dplyr::left_join(
    janno_final %>% dplyr::select(-region_id),
    by = c("search_id" = "Individual_ID")
  ) %>%
  dplyr::mutate(
    Pop = sapply(Group_Name, \(x) x[[1]]),
    Pup = sapply(Publication_Status, \(x) x[[1]])
  )


#### mobility estimator curves ####
p_estimator <- ggplot() +
  lemon::facet_rep_wrap(~region_id, ncol = 2, repeat.tick.labels = T) +
  geom_point(
    data = origin_grid_mean_infodense,
    mapping = aes(
      x = mean_search_z, y = undirected_mean_spatial_distance, color = mean_angle_deg,
      label1 = search_id, label2 = Pop, label3 = Pup
    ),
    alpha = 1,
    size = 1.5,
    shape = 4
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
  ) +
  xlab("time in years calBC/calAD") +
  ylab("spatial distance to \"link point\" (undirected mean) [km]") +
  scale_color_gradientn(
    colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"),
    na.value = NA,
    guide = F
  ) +
  scale_x_continuous(breaks = seq(-7000, 1000, 1000)) +
  coord_cartesian(
    xlim = c(-7400, 1400),
    ylim = c(-100, max(origin_grid_mean_infodense$undirected_mean_spatial_distance, na.rm = T))
  )

p_estimator_plotly <- p_estimator %>% plotly::ggplotly(
  tooltip = paste0("label", 1:3)
)

htmlwidgets::saveWidget(p_estimator_plotly, "plots/interactive_mobility_exploration.html")

