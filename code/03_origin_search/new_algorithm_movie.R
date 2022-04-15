library(magrittr)

#### data ####

load("data/genotype_data/janno_final.RData")
load("data/spatial/search_area.RData")
load("data/origin_search/default_kernel.RData")

#### prepare model grid ####

janno_search <- janno_final %>%
  dplyr::mutate(
    search_z = sapply(janno_final$Date_BC_AD_Sample, function(x){ x[1] })
  ) %>% dplyr::filter(
    Poseidon_ID %in% c(
      #"Stuttgart_published.DG"#, 
      "RISE434.SG"#, 
      # "3DT26.SG",
      # "SI-40.SG"
    )
  )

search <- mobest::locate(
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
    C2_mds_u = janno_final$C2_mds_u#,
    #C3_mds_u = janno_final$C3_mds_u
  ),
  kernel = mobest::create_kernset_multi(
    kernel_1 = mobest::create_kernset(
      C1_mds_u = mobest::create_kernel(900000, 900000, 800, 0.07),
      C2_mds_u = mobest::create_kernel(900000, 900000, 800, 0.07)#,
      #C3_mds_u = mobest::create_kernel(900000, 900000, 800, 0.2)
    )
  ),
  search_independent = mobest::create_spatpos_multi(
    search_selection = mobest::create_spatpos(
      id = janno_search$Poseidon_ID,
      x = janno_search$x,
      y = janno_search$y,
      z = janno_search$Date_BC_AD_Median_Derived
    )
  ),
  search_dependent = mobest::create_obs(
    C1_mds_u = janno_search$C1_mds_u,
    C2_mds_u = janno_search$C2_mds_u#,
    #C3_mds_u = janno_search$C3_mds_u
  ),
  search_space_grid = mobest::create_prediction_grid(
    search_area,
    spatial_cell_size = 70000
  ),
  search_time = seq(-4000, -2000, 50),
  search_time_mode = "absolute"
)

gugu <- mobest::multiply_dependent_probabilities(search)

huhu <- gugu %>%
  dplyr::filter(
    #id == "Stuttgart_published.DG",
    #id == "3DT26.SG",
    independent_table_id == "search_selection",
    field_independent_table_id == "age_median",
    field_kernel_setting_id == "kernel_1"
  )

library(ggplot2)
huhu %>% dplyr::group_split(search_z) %>%
  purrr::walk2(
    1:length(.), .,
    function(i, x) {
      p <- x %>%
        ggplot() +
        geom_raster(
          aes(x = field_x, y = field_y, fill = probability_product)
        ) +
        ggtitle(x$search_z %>% unique)
      ggsave(
        paste0("plots/presentation/movie/", sprintf("sequence_%03d", i), ".jpeg"),
        p,
        width = 10,
        height = 7
      )
    }
  )

# ffmpeg -f image2 -framerate 3 -i sequence_%03d.jpeg -loop 0 -s 720x480 sequence.gif
