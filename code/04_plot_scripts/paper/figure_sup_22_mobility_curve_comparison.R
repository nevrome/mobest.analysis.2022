library(magrittr)
library(ggplot2)

load("data/origin_search/packed_origin_vectors.RData")

prep <- packed_origin_vectors %>%
  dplyr::select(search_id, region_id, multivar_method, search_time, search_z, ov_dist) %>%
  dplyr::filter(region_id != "Other region" )

ovs <- dplyr::left_join(
    prep,
    prep %>% dplyr::select(-region_id, -search_z),
    by = c("search_id")
  ) %>%
  dplyr::filter(
    multivar_method.x == "mds2" & search_time.x == -667 &
      ( (multivar_method.y == "mds2" & search_time.y != -667 & search_time.x != search_time.y) |
          (multivar_method.y == "pca5" & search_time.y == -667 & search_time.x == search_time.y) )
  ) %>% 
  dplyr::mutate(
    run_raw = paste(multivar_method.y, search_time.y),
    run = dplyr::case_when(
      run_raw == "mds2 -1015" ~ "MDS2 High rearview distance",
      run_raw == "mds2 -378" ~ "MDS2 Low rearview distance",
      run_raw == "pca5 -667" ~ "PCA5"
    ),
    ovs_dist = ov_dist.y - ov_dist.x
  ) %>%
  # sort by ovs_dist
  dplyr::arrange(dplyr::desc(ovs_dist))

p <- ovs %>% ggplot() +
  facet_grid(rows = dplyr::vars(region_id), cols = dplyr::vars(run)) +
  geom_segment(
    mapping = aes(x = search_z, xend = search_z, y = ov_dist.x, yend = ov_dist.y, color = ovs_dist),
    alpha = 0.7,
    size = 1
  ) +
  # geom_point(
  #   mapping = aes(
  #     x = search_z, y = ov_dist.y, color = ovs_dist
  #   ),
  #   alpha = 1,
  #   size = 0.3,
  #   shape = 20
  # ) +
  geom_point(
    mapping = aes(
      x = search_z, y = ov_dist.y
    ),
    alpha = 1,
    size = 0.3,
    shape = 20,
    color = "black"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +   
  coord_cartesian(
    xlim = c(-7500, 1500),
    ylim = c(-100, 3000)
  ) +
  scale_x_continuous(breaks = seq(-7000, 1000, 2000)) +
  scale_color_gradient2(
    low = "#0571b0",
    mid = "white",
    high = "#ca0020"
  ) +
  guides(
    color = guide_colorbar(
      title = "Divergence from MDS2 with the default rearview distance [km]    ",
      barwidth = 20, barheight = 1.5
    )
  ) +
  xlab("time in years calBC/calAD") +
  ylab("spatial distance to \"origin point\" (directed mean) in km") 

ggsave(
  paste0("plots/figure_sup_22_mobility_curve_comparison.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.6,
  dpi = 300,
  width = 430, height = 420, units = "mm",
  limitsize = F
)
