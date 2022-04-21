# qsub -b y -cwd -q archgen.q -pe smp 16 -l h_vmem=50G -now n -V -j y -o ~/log -N apple singularity exec --bind=/mnt/archgen/users/schmid singularity_mobest.sif Rscript code/04_plot_scripts/presentations/origin_search_narrative_movie.R

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

plot_prob <- function(search_prod, path) {
  
  p <- ggplot() +
    geom_sf(data = extended_area, fill = "black") +
    geom_raster(
      data = search_prod,
      mapping = aes(x = field_x, y = field_y, fill = probability_product),
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
    ) +
    ggtitle(paste("time calBC/AD:", unique(search_prod$search_z)))
  
  ggsave(
    path,
    plot = p,
    device = "png",
    scale = 0.6,
    dpi = 300,
    width = 350, height = 300, units = "mm",
    limitsize = F
  )
  
}

spatial_pred_grid <- mobest::create_prediction_grid(
  area,
  spatial_cell_size = 20000
)

search_movie <- mobest::locate(
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
    id = janno_search$Poseidon_ID,
    x = janno_search$x,
    y = janno_search$y,
    z = janno_search$Date_BC_AD_Median_Derived
  ),
  search_dependent = mobest::create_obs(
    C1_mds_u = janno_search$C1_mds_u,
    C2_mds_u = janno_search$C2_mds_u
  ),
  search_space_grid = spatial_pred_grid,
  search_time = seq(-8000, -5000, 20),
  search_time_mode = "absolute"
)

search_movie_prod <- mobest::multiply_dependent_probabilities(search_movie)

search_movie_prod %>%
  dplyr::group_split(search_z) %>%
  purrr::walk2(
    ., paste0("plots/presentation/movie/", sprintf("frame_%03d", 1:length(.))),
    plot_prob
  )

