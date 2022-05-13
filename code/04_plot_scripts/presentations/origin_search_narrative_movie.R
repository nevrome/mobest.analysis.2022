# qsub -b y -cwd -q archgen.q -pe smp 32 -l h_vmem=50G -now n -V -j y -o ~/log -N apple singularity exec --bind=/mnt/archgen/users/schmid singularity_mobest.sif Rscript code/04_plot_scripts/presentations/origin_search_narrative_movie.R

library(magrittr)
library(ggplot2)

load("data/genotype_data/janno_final.RData")
load("data/spatial/area.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg3035.RData")
load("data/origin_search/default_kernset_mds2.RData")

janno_search <- janno_final %>%
  dplyr::mutate(
    search_z = Date_BC_AD_Median_Derived
  ) %>% dplyr::filter(
    Poseidon_ID %in% c(
      "3DT26.SG"
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
    facet_wrap(
      ~search_id,
      ncol = 2,
      labeller = ggplot2::labeller(search_id = c(
        "Stuttgart_published.DG" = paste(
          "Stuttgart ~5250BC",
          "Early Neolithic, Linear Pottery culture",
          "Lazaridis et al. 2014",
          sep = "\n"
        ),
        "RISE434.SG" = paste(
          "RISE434 ~2750BC",
          "Late Neolithic, Corded Ware culture",
          "Allentoft et al. 2015",
          sep = "\n"
        ),
        "3DT26.SG" = paste(
          "3DRIF-26 ~200AD",
          "Roman Britain",
          "Martiniano et al. 2016",
          sep = "\n"
        ),
        "SI-40.SG" = paste(
          "SI-40 ~1150AD",
          "Medieval Period, Crusades",
          "Haber et al. 2019",
          sep = "\n"
        )
      ))
    ) +
    geom_sf(data = extended_area, fill = "black") +
    geom_raster(
      data = search_prod,
      mapping = aes(x = field_x, y = field_y, fill = probability),
    ) +
    annotate(
      "text",
      x = 6700000, y = 4400000, size = 11,
      label = toBCAD(unique(search_prod$field_z))
    ) +
    scale_fill_viridis_c(breaks = c(500, 1000), option = "mako", direction = -1) +
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
      fill = guide_colorbar(title = "\"Origin\" probability  ", barwidth = 25)
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
    width = 325, height = 300, units = "mm",
    limitsize = F
  )
  
}

spatial_pred_grid <- mobest::create_prediction_grid(
  area,
  spatial_cell_size = 20000
)

#### movie1 ####

search_movie1 <- mobest::locate(
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
  kernel = default_kernset_mds2,
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
  search_time = seq(-1000, 300, 10),
  search_time_mode = "absolute"
)

search_movie1_prod <- mobest::multiply_dependent_probabilities(search_movie1)

search_movie1_prod %>%
  dplyr::group_split(field_z) %>%
  purrr::walk2(
    ., paste0("plots/presentation/movie/", sprintf("frame_movie3_%03d", 1:length(.)), ".jpeg"),
    plot_prob
  )

# ffmpeg -r 10 -f image2 -s 720x480 -i frame_movie3_%03d.jpeg -vf "pad=ceil(iw/2)*2:ceil(ih/2)*2" -vcodec libx264 -crf 40 -pix_fmt yuv420p example_movie.mp4
