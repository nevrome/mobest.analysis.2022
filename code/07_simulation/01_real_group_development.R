library(magrittr)
library(ggplot2)

load("data/genotype_data/janno_final.RData")
load("data/spatial/epsg3035.RData")
load("data/spatial/area.RData")
load("data/spatial/area.RData")
load("data/spatial/extended_area.RData")
load("data/origin_search/default_kernset_mds2.RData")
load("data/plot_reference_data/age_colors_gradient.RData")

janno_sf <- sf::st_as_sf(janno_final, coords = c("x", "y"), crs = epsg3035)
circle_centers <- tibble::tribble(
  ~setup, ~group, ~location, ~x, ~y,
  "I",   "A", "Iberia",      3100000, 2000000,
  "I",   "B", "Baltics",     5400000, 3900000,
  "II",  "A", "Britain",     3400000, 3500000,
  "II",  "B", "Balkans",     5300000, 2500000,
  "III", "A", "Scandinavia", 4500000, 3800000,
  "III", "B", "Italy",       4500000, 2000000
)
circle_centers_sf <- circle_centers %>%
  sf::st_as_sf(coords = c("x", "y"), crs = epsg3035)
circles <- circle_centers_sf %>%
  sf::st_buffer(
    # dist = 480000,
    # dist = 300000,
    # dist = 250000
    dist = 500000
  )

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
  geom_sf(data = circles, aes(color = group, fill = group), alpha = 0.3, size = 2) +
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

inter <- sf::st_intersection(
  janno_sf,
  circles
)

#### prepare model grid ####
times <- seq(-7500, 1500, 100)

model_grid <- mobest::create_model_grid(
  independent = mobest::create_spatpos_multi(
    age_median = mobest::create_spatpos(
      id = janno_final$Poseidon_ID,
      x = janno_final$x,
      y = janno_final$y,
      z = janno_final$Date_BC_AD_Median_Derived
    )
  ),
  dependent = mobest::create_obs_multi(
    MDS2 = mobest::create_obs(
      C1_mds_u = janno_final$C1_mds_u
    )
  ),
  kernel = mobest::create_kernset_multi(
    default_kernel = default_kernset_mds2
  ),
  prediction_grid = mobest::create_spatpos_multi(
    circles = mobest::create_geopos(
      id = circle_centers$location,
      x = circle_centers$x,
      y = circle_centers$y
    ) %>% mobest::geopos_to_spatpos(times)
  )
)

interpol_grid_examples <- mobest::run_model_grid(model_grid)

ie <- dplyr::left_join(
  interpol_grid_examples,
  circle_centers,
  by = c("geo_id" = "location", "x", "y")
)

p_funnel <- ggplot() +
  facet_wrap(~setup) +
  geom_point(
    data = inter,
    aes(x = Date_BC_AD_Median_Derived, y = C1_mds_u, color = group)
  ) +
  geom_ribbon(
    data = ie,
    aes(x = z, ymin = mean - sd, ymax = mean + sd, fill = group),
    alpha = 0.2
  ) +
  geom_line(
    data = ie,
    aes(x = z, y = mean, color = group)
  ) +
  scale_colour_manual(values =  c("A" = "#F8766D", "B" = "#00BFC4")) +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  xlab("time [years calBC/calAD]")

cowplot::plot_grid(
  p_map,
  p_funnel,
  nrow = 2,
  align = "v"
)