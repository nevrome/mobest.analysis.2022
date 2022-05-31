load("data/simulation/mock_data.RData")

#### search test run ####

locate_test_res <- mobest::locate_multi(
  independent = mobest::create_spatpos_multi(A = independent_list_II[[2]][[6]]),
  dependent = dependent_list_III[[2]][[6]],
  kernel = mobest::create_kernset_multi(A = kernels_list$kernel_3),
  search_independent = mobest::create_spatpos_multi(
    A = mobest::create_spatpos(id = "pioneer", x = 0.75, y = 0.25, z = 1)
  ),
  search_dependent = mobest::create_obs_multi(
    limited_slow = mobest::create_obs(component = 0.25),
    limited_fast = mobest::create_obs(component = 0.25),
    intertwined  = mobest::create_obs(component = 0.25)
  ),
  # spatial search grid: Where to search
  search_space_grid = expand.grid(
    x = seq(0, 1, 0.1), 
    y = seq(0, 1, 0.1)
  ) %>% { mobest::create_geopos(id = 1:nrow(.), x = .$x, y = .$y) },
  # search time: When to search
  search_time = seq(0.1,0.9,0.1),
  search_time_mode = "absolute",
  quiet = T
)

locate_test_product <- mobest::multiply_dependent_probabilities(locate_test_res)
ovs <- mobest::determine_origin_vectors(locate_test_product, dependent_setting_id, field_z)

p_list <- purrr::map2(
  dplyr::group_split(locate_test_product, dependent_setting_id),
  dplyr::group_split(ovs, dependent_setting_id),
  function(locate_dep, ovs_dep) {
    ggplot() +
      facet_wrap(~field_z) +
      geom_raster(
        data = locate_dep,
        mapping = aes(x = field_x, y = field_y, fill = probability)
      ) +
      geom_point(
        data = mobest::create_spatpos(id = "pioneer", x = 0.75, y = 0.25, z = 1),
        mapping = aes(x = x, y = y),
        colour = "red"
      ) +
      geom_point(
        data = ovs_dep,
        mapping = aes(x = field_x, y = field_y),
        colour = "orange"
      ) +
      coord_fixed() +
      theme_bw() +
      theme(legend.position = "none")
    }
  )

cowplot::plot_grid(
  plotlist = p_list, nrow = 1
)
