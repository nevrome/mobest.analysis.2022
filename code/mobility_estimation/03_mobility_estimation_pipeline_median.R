library(magrittr)

#### data ####

load("data/poseidon_data/janno_final.RData")
load("data/spatial/area.RData")

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
    #ds350_dt350_g006 = list(d = c(350000, 350000, 350), g = 0.06, on_residuals = T, auto = F)
    ds550_dt1050_g006 = list(d = c(550000, 550000, 1050), g = 0.06, on_residuals = T, auto = F)#,
    #ds800_dt400_g001 = list(d = c(800000, 800000, 400), g = 0.01, on_residuals = T, auto = F)
  ),
  prediction_grid = list(
    scs100_tl100 = mobest::create_prediction_grid(
      area,
      mobility_regions,
      spatial_cell_size = 100000,
      time_layers = seq(-7500, 1500, 100)
    )
    # scs200_tl200 = mobest::create_prediction_grid(
    #   area, 
    #   spatial_cell_size = 150000, 
    #   time_layers = seq(-7500, 1500, 281.25)
    # )
  )
)

#### run interpolation on model grid ####

interpol_grid <- mobest::run_model_grid(model_grid)

library(ggplot2)
interpol_grid %>%
  dplyr::filter(
    #kernel_setting_id == "ds400_dt700_g001",
    dependent_var_id == "C2",
    z %in% seq(-7000, -2000, 500)
    #z %% 500 == 0
  ) %>%
  ggplot() +
  geom_raster(aes(x, y, fill = mean, alpha = sd)) +
  facet_wrap(~z) +
  scale_fill_viridis_c() +
  scale_alpha_continuous(range = c(1, 0), na.value = 0)
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

interpol_grid_origin <- mobest::search_spatial_origin(interpol_grid, steps = 1)

gb <- interpol_grid_origin %>%
  dplyr::filter(
    region_id == "Iberia"
  )

load("data/spatial/extended_area.RData")

library(ggplot2)
gb %>% dplyr::filter(
  #z > -4000 & z < -3000
  z %% 500 == 0
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

mobility_proxy <- mobest::estimate_mobility(interpol_grid_origin)

#mobility <- mobility_proxy
 
#save(mobility_proxy, file = paste0("data/mobility_estimation/mobility_proxy_median.RData"))

mobility_proxy %>%
  # main
  dplyr::group_by(region_id, z, independent_table_id, kernel_setting_id) %>%
  dplyr::summarise(
    mean_speed_km_per_decade = mean(speed_km_per_decade)#,
    #mean_angle_deg = mobest::mean_deg(angle_deg)
  ) %>%
  ggplot() +
  geom_line(
    aes(
      x = z, y = mean_speed_km_per_decade,
      group = interaction(independent_table_id, kernel_setting_id)#,
      #color = mean_angle_deg
    ),
    alpha = 0.5
  ) +
  facet_grid(cols = dplyr::vars(region_id), rows = dplyr::vars(kernel_setting_id)) +
   scale_color_gradientn(
    colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A")
  )


#### boxplot ####

interpol_grid_origin %>% dplyr::mutate(
  z_cut = cut(z, breaks = seq(-7500, 1500, 500), labels = seq(-7250, 1250, 500))
) %>% ggplot() +
  geom_boxplot(
    aes(
      x = z_cut, y = spatial_distance, group = z_cut#,
      #group = interaction(independent_table_id, kernel_setting_id),
      #color = angle_deg
    )
  ) +
  facet_wrap(dplyr::vars(region_id))

# mobility_proxy %>%
#   dplyr::group_by(kernel_setting_id, region_id) %>%
#   dplyr::arrange(z, .by_group = T) %>%
#   dplyr::mutate(
#     movavg = slider::slide_dbl(mean_km_per_decade, mean, .before = 4, .after = 4)
#   ) %>%
#   ggplot() +
#   geom_line(
#     aes(
#       x = z, y = mean_km_per_decade,
#       group = interaction(independent_table_id, kernel_setting_id),
#       color = angle_deg
#     ),
#     alpha = 0.5
#   ) +
#   geom_line(
#     aes(
#       x = z, y = movavg,
#       group = interaction(independent_table_id, kernel_setting_id),
#     ),
#     color = "blue"
#   ) +
#   #facet_grid(cols = dplyr::vars(region_id), rows = dplyr::vars(kernel_setting_id)) +
#   facet_wrap(dplyr::vars(region_id)) +
#   theme_bw() +
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_text(angle = 40, hjust = 1),
#     strip.background = element_rect(fill = NA)
#   ) +
#   xlab("time calBC/calAD [y]") +
#   ylab("\"Speed\" [km/decade]") +
#   scale_color_gradientn(
#     colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"),
#     guide = F
#   ) +
#   scale_x_continuous(breaks = c(-7000, -5000, -3000, -1000, 1000))

