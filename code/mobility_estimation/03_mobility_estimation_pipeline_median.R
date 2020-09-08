library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")
load("data/spatial/mobility_regions.RData")

#### prepare pca model grid ####
model_grid <- mobest::create_model_grid(
  independent = list(
      tibble::tibble(
        x = janno_final$x, 
        y = janno_final$y, 
        z = janno_final$Date_BC_AD_Median_Derived
      )
    ) %>% stats::setNames("age_median"),
  dependent = list(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  kernel = list(
    #ds400_dt200_g001 = list(d = c(400000, 400000, 200), g = 0.01, on_residuals = T, auto = F),
    ds600_dt300_g001 = list(d = c(600000, 600000, 300), g = 0.01, on_residuals = T, auto = F)#,
    #ds800_dt400_g001 = list(d = c(800000, 800000, 400), g = 0.01, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    scs100_tl100 = mobest::create_prediction_grid(
      area,
      spatial_cell_size = 100000,
      time_layers = seq(-7500, 1500, 50)
    )
    # scs200_tl200 = mobest::create_prediction_grid(
    #   area, 
    #   spatial_cell_size = 150000, 
    #   time_layers = seq(-7500, 1500, 281.25)
    # )
  )
)

#### run interpolation on model grid ####

model_grid_result <- mobest::run_model_grid(model_grid)

#### unnest prediction to get a point-wise prediction table ####

interpol_grid <- mobest::unnest_model_grid(model_grid_result)

# library(ggplot2)
# interpol_grid %>%
#   dplyr::filter(
#     #kernel_setting_id == "ds400_dt700_g001",
#     dependent_var_id == "C1",
#     z %% 500 == 0
#   ) %>%
#   ggplot() +
#   geom_raster(aes(x, y, fill = mean)) +
#   facet_wrap(~z) +
#   scale_fill_viridis_c()
# 
# interpol_grid %>%
#   dplyr::filter(
#     #kernel_setting_id == "ds400_dt700_g001",
#     dependent_var_id == "C2",
#     z %% 200 == 0
#   ) %>%
#   ggplot() +
#   geom_raster(aes(x, y, fill = mean)) +
#   facet_wrap(~z) +
#   scale_fill_viridis_c()


#save(interpol_grid, file = "data/gpr/interpol_grid.RData")
#save(interpol_grid, file = "data/gpr/interpol_grid_median.RData")

#### spatial origin ####

interpol_grid_origin <- mobest::search_spatial_origin(interpol_grid, steps = 4)

gb <- interpol_grid_origin %>%
  sf::st_as_sf(
    coords = c("x", "y"),
    crs = sf::st_crs(mobility_regions),
    remove = FALSE
  ) %>%
  sf::st_intersection(mobility_regions) %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(
    region_id == "Britain and Ireland"
  )

load("data/spatial/extended_area.RData")

gb %>% dplyr::filter(
  z > -4000 & z < -3500
) %>%
  ggplot() +
  geom_sf(data = extended_area) +
  geom_raster(aes(x, y, fill = mean_C1)) +
  geom_segment(aes(x, y, xend = x_origin, yend = y_origin)) +
  facet_wrap(~z)

# library(ggplot2)
# interpol_grid_origin %>%
#   dplyr::filter(
#     kernel_setting_id == "ds600_dt300_g001",
#     z == -5500
#   ) %>% ggplot() +
#   geom_segment(
#     aes(x, y, xend = x_origin, yend = y_origin)
#   )

#### mobility proxy ####

mobility_proxy <- mobest::estimate_mobility(interpol_grid_origin, mobility_regions)

mobility <- mobility_proxy
 
save(mobility_proxy, file = paste0("data/mobility_estimation/mobility_proxy_median.RData"))

mobility_proxy %>%
  dplyr::group_by(kernel_setting_id, region_id) %>%
  dplyr::arrange(z, .by_group = T) %>%
  dplyr::mutate(
    movavg = slider::slide_dbl(mean_km_per_decade, mean, .before = 4, .after = 4)
  ) %>%
  ggplot() +
  geom_line(
    aes(
      x = z, y = mean_km_per_decade,
      group = interaction(independent_table_id, kernel_setting_id),
      color = angle_deg
    ),
    alpha = 0.5
  ) +
  geom_line(
    aes(
      x = z, y = movavg,
      group = interaction(independent_table_id, kernel_setting_id),
    ),
    color = "blue"
  ) +
  #facet_grid(cols = dplyr::vars(region_id), rows = dplyr::vars(kernel_setting_id)) +
  facet_wrap(dplyr::vars(region_id)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 40, hjust = 1),
    strip.background = element_rect(fill = NA)
  ) +
  xlab("time calBC/calAD [y]") +
  ylab("\"Speed\" [km/decade]") +
  scale_color_gradientn(
    colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"),
    guide = F
  ) +
  scale_x_continuous(breaks = c(-7000, -5000, -3000, -1000, 1000))
