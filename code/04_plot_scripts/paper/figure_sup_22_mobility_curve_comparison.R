library(magrittr)

get_mean_points <- function(x, run) {
  x %>%
    dplyr::select(mean_search_z, search_id, region_id, directed_mean_spatial_distance) %>%
    dplyr::mutate(run = run)
}

load("data/origin_search/origin_grid_mean.RData")
mds2 <- origin_grid_mean %>% get_mean_points("Default: 2D MDS")
load("data/origin_search/origin_grid_mean_mds3.RData")
mds3 <- origin_grid_mean %>% get_mean_points("3D MDS")
load("data/origin_search/origin_grid_derived_data_retro_low.RData")
low_retro <- origin_grid_derived_data$origin_grid_mean %>% get_mean_points("Lower rearview distance")
load("data/origin_search/origin_grid_derived_data_retro_high.RData")
high_retro <- origin_grid_derived_data$origin_grid_mean %>% get_mean_points("Higher rearview distance")

comb <- dplyr::bind_rows(mds3, low_retro, high_retro) %>%
  dplyr::left_join(mds2 %>% dplyr::select(search_id, spacedist = directed_mean_spatial_distance)) %>%
  dplyr::mutate(diff_to_mds2 = directed_mean_spatial_distance - spacedist) %>%
  # order by divergence, to highlight stronger colours in plot
  dplyr::group_by(run, region_id) %>%
  dplyr::arrange(abs(diff_to_mds2), .by_group = TRUE) %>%
  dplyr::ungroup()

library(ggplot2)

p <- ggplot() +
  facet_grid(rows = dplyr::vars(region_id), cols = dplyr::vars(run)) +
  geom_point(
    data = comb,
    mapping = aes(
      x = mean_search_z, y = directed_mean_spatial_distance, fill = diff_to_mds2
    ),
    alpha = 1,
    size = 1.7,
    shape = 21,
    colour = "black",
    stroke = 0.15
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +   
  coord_cartesian(
    xlim = c(-7400, 1400),
    ylim = c(-100, 3000)
  ) +
  scale_x_continuous(breaks = seq(-7000, 1000, 2000)) +
  scale_fill_gradient2(
    low = "#0571b0",
    mid = "white",
    high = "#ca0020"
  ) +
  guides(
    fill = guide_colorbar(title = "Divergence from default [km]    ", barwidth = 20, barheight = 1.5),
  ) +
  xlab("time [years calBC/calAD]") +
  ylab("spatial distance to \"origin point\" (directed mean) [km]") 

ggsave(
  paste0("plots/figure_sup_22_mobility_curve_comparison.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 430, height = 390, units = "mm",
  limitsize = F
)
