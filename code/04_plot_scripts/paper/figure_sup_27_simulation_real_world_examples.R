library(ggplot2)

load("data/genotype_data/janno_final.RData")
load("data/simulation/real_world_group_development.RData")
load("data/plot_reference_data/age_colors_gradient.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")

p_map <- ggplot() +
  facet_wrap(~setup) +
  geom_sf(
    data = extended_area,
    fill = "white", colour = "darkgrey", size = 0.4
  ) +
  geom_jitter(
    data = janno_final %>% dplyr::arrange(Date_BC_AD_Median_Derived),
    aes(x = x, y = y, color = Date_BC_AD_Median_Derived),
    size = 0.5,
    width = 60000,
    height = 60000
  ) +
  age_colors_gradient +
  ggnewscale::new_scale_color() +
  geom_sf(data = circle_centers_sf, aes(color = group), size = 5) +
  geom_sf(data = circles_sf, aes(color = group, fill = group), alpha = 0.3, size = 2) +
  scale_colour_manual(values = c("A" = "#F8766D", "B" = "#00BFC4")) +
  theme_bw() +
  coord_sf(
    expand = FALSE,
    crs = epsg3035
    #, datum = epsg3035
  ) + 
  theme(
    axis.title = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "#BFD5E3")
  )

p_funnel <- ggplot() +
  facet_wrap(~setup) +
  geom_point(
    data = janno_intersected,
    aes(x = Date_BC_AD_Median_Derived, y = C1_mds_u, color = group)
  ) +
  geom_ribbon(
    data = interpolation_circle_centers,
    aes(x = z, ymin = mean - sd, ymax = mean + sd, fill = group),
    alpha = 0.2
  ) +
  geom_line(
    data = interpolation_circle_centers,
    aes(x = z, y = mean, color = group)
  ) +
  scale_colour_manual(values =  c("A" = "#F8766D", "B" = "#00BFC4")) +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  xlab("time [years calBC/calAD]")

p <- cowplot::plot_grid(
  p_map,
  p_funnel,
  nrow = 2,
  align = "v"
)

ggsave(
  paste0("plots/figure_sup_27_simulation_real_world_examples.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.8,
  dpi = 300,
  width = 350, height = 200, units = "mm",
  limitsize = F
)
