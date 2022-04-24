# qsub -b y -cwd -q archgen.q -pe smp 32 -l h_vmem=50G -now n -V -j y -o ~/log -N apple singularity exec --bind=/mnt/archgen/users/schmid singularity_mobest.sif Rscript code/04_plot_scripts/presentations/origin_search_narrative_movie.R

library(magrittr)
library(ggplot2)

load("data/genotype_data/janno_final.RData")
load("data/spatial/area.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")

janno_search <- janno_final %>%
  dplyr::mutate(
    search_z = Date_BC_AD_Median_Derived
  ) %>% dplyr::filter(
    Poseidon_ID %in% c(
      "Stuttgart_published.DG"
    )
  )

toBCAD <- function(x) {
  dplyr::case_when(
    x < 0 ~ paste(abs(x), "calBC"),
    x == 0 ~ paste(x, "calBC/AD"),
    x > 0 ~ paste(x, "calAD")
  )
}

plot_prob <- function(search_prod, path) {
  
  p <- ggplot() +
    geom_sf(data = extended_area, fill = "black") +
    geom_raster(
      data = search_prod,
      mapping = aes(x = field_x, y = field_y, fill = probability_product),
    ) +
    annotate(
      "text",
      x = 6700000, y = 4400000, size = 11,
      label = toBCAD(unique(search_prod$search_z))
    ) +
    scale_fill_viridis_c(option = "mako", direction = -1) +
    geom_sf(data = extended_area, fill = NA, colour = "black") +
    geom_point(
      data = janno_search,
      mapping = aes(x = x, y = y),
      colour = "red",
      size = 5
    ) +
    theme_bw() +
    coord_sf(
      expand = FALSE,
      crs = epsg3035
    ) +
    guides(
      fill = guide_colorbar(title = "Probability  ", barwidth = 25)
    ) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = 15),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.text = element_text(size = 15),
      strip.text = element_text(size = 15),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "#BFD5E3")
    )
  
  ggsave(
    path,
    plot = p,
    device = "jpeg",
    scale = 0.6,
    dpi = 300,
    width = 370, height = 300, units = "mm",
    limitsize = F
  )
  
}

spatial_pred_grid <- mobest::create_prediction_grid(
  area,
  spatial_cell_size = 20000
)

#### movie1 ####

# search_movie1 <- mobest::locate(
#   independent = mobest::create_spatpos(
#     id = janno_final$Poseidon_ID,
#     x = janno_final$x,
#     y = janno_final$y,
#     z = janno_final$Date_BC_AD_Median_Derived
#   ),
#   dependent = mobest::create_obs(
#     C1_mds_u = janno_final$C1_mds_u,
#     C2_mds_u = janno_final$C2_mds_u
#   ),
#   kernel = mobest::create_kernset(
#     C1_mds_u = mobest::create_kernel(800000, 800000, 800, 0.07),
#     C2_mds_u = mobest::create_kernel(800000, 800000, 800, 0.07)
#   ),
#   search_independent = mobest::create_spatpos(
#     id = janno_search$Poseidon_ID,
#     x = janno_search$x,
#     y = janno_search$y,
#     z = janno_search$Date_BC_AD_Median_Derived
#   ),
#   search_dependent = mobest::create_obs(
#     C1_mds_u = janno_search$C1_mds_u,
#     C2_mds_u = janno_search$C2_mds_u
#   ),
#   search_space_grid = spatial_pred_grid,
#   search_time = seq(-7500, -5000, 10),
#   search_time_mode = "absolute"
# )
# 
# search_movie1_prod <- mobest::multiply_dependent_probabilities(search_movie1)
# 
# search_movie1_prod %>%
#   dplyr::group_split(search_z) %>%
#   purrr::walk2(
#     ., paste0("plots/presentation/movie/", sprintf("frame_movie1_%03d", 1:length(.)), ".jpeg"),
#     plot_prob
#   )

#### movie2 ####

janno_search <- tibble::tibble(x = 25.90848, y = 42.51272) %>% ggplot2::sf_transform_xy(epsg3035, 4326)

model_grid <- mobest::create_model_grid(
  independent = mobest::create_spatpos_multi(
    age_median = mobest::create_spatpos(
      id = janno_final$Poseidon_ID,
      x = janno_final$x,
      y = janno_final$y,
      z = janno_final$Date_BC_AD_Median_Derived
    )
  ),
  dependent = mobest::create_obs(
    C1_mds_u = janno_final$C1_mds_u,
    C2_mds_u = janno_final$C2_mds_u
  ),
  kernel = mobest::create_kernset_multi(
    kernel1 = mobest::create_kernset(
      C1_mds_u = mobest::create_kernel(800000, 800000, 800, 0.07),
      C2_mds_u = mobest::create_kernel(800000, 800000, 800, 0.07)
    )
  ),
  prediction_grid = mobest::create_spatpos_multi(
    poi = mobest::create_geopos(
      id = "Karanovo",
      x = janno_search$x,
      y = janno_search$y
    ) %>% mobest::geopos_to_spatpos(seq(-7500, -1500, 10))
  )
)

interpol_grid <- mobest::run_model_grid(model_grid)

interpol_grid_wide <- interpol_grid %>%
  tidyr::pivot_wider(
    names_from = "dependent_var_id",
    values_from = c("mean", "sd")
  )

search_movie2 <- mobest::locate(
  independent = mobest::create_spatpos(
    id = janno_final$Poseidon_ID,
    x = janno_final$x,
    y = janno_final$y,
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = mobest::create_obs(
    C1_mds_u = janno_final$C1_mds_u,
    C2_mds_u = janno_final$C2_mds_u
  ),
  kernel = mobest::create_kernset(
    C1_mds_u = mobest::create_kernel(800000, 800000, 800, 0.07),
    C2_mds_u = mobest::create_kernel(800000, 800000, 800, 0.07)
  ),
  search_independent = mobest::create_spatpos(
    id = interpol_grid_wide$id,
    x = interpol_grid_wide$x,
    y = interpol_grid_wide$y,
    z = interpol_grid_wide$z
  ),
  search_dependent = mobest::create_obs(
    C1_mds_u = interpol_grid_wide$mean_C1_mds_u,
    C2_mds_u = interpol_grid_wide$mean_C2_mds_u
  ),
  search_dependent_error = mobest::create_obs_error(
    C1_mds_u_sd = interpol_grid_wide$sd_C1_mds_u,
    C2_mds_u_sd = interpol_grid_wide$sd_C2_mds_u
  ),
  search_space_grid = spatial_pred_grid,
  search_time = 0,
  search_time_mode = "relative"
)

search_movie2_prod <- mobest::multiply_dependent_probabilities(search_movie2)

search_movie2_prod %>%
  dplyr::group_split(search_z) %>%
  purrr::walk2(
    ., paste0("plots/presentation/movie/", sprintf("frame_movie2_%03d", 1:length(.))),
    plot_prob
  )

