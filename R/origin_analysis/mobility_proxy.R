library(magrittr)

load("data/gpr/gpr_pred_grid_temporal_sampling_v3.RData")
load("data/pri_ready.RData")
load("data/spatial/mobility_regions.RData")

points_regions <- pred_grid %>% 
  dplyr::select(x_real, y_real, point_id) %>%
  sf::st_as_sf( 
    coords = c("x_real", "y_real"), 
    crs = "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs"
  ) %>%
  sf::st_intersection(mobility_regions) %>%
  tibble::as_tibble() %>%
  dplyr::select(-geometry)

pri <- pri_ready %>%
  dplyr::left_join(points_regions, by = "point_id")

pri_mean <- pri %>%
  dplyr::group_by(
    independent_table_id, kernel_setting_id, age_sample, region_id
  ) %>%
  dplyr::summarise(
    mean_km_per_decade = mean(spatial_distance)/1000/10,
    mean_angle = mean(circular::circular(angle_degree, type="angles", units="degrees",modulo="2pi", template='geographics'), na.rm = T)
  ) %>%
  dplyr::ungroup()

save(pri_mean, file = "data/mobility_estimation/pri_mean.RData")
