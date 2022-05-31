load("data/simulation/mock_data.RData")

#### search test run ####

locate_test_res <- mobest::locate(
  independent = independent_list_II[[2]][[6]],
  dependent = dependent_list_III[[2]][[6]]$intertwined,
  kernel = kernels_list$kernel_3,
  search_independent = mobest::create_spatpos(id = "pioneer", x = 0.75, y = 0.25, z = 1),
  search_dependent = mobest::create_obs(component = 0.25),
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
ovs_test <- mobest::determine_origin_vectors(locate_test_product, field_z)

ggplot() +
  facet_wrap(~field_z) +
  geom_raster(
    data = locate_test_res,
    mapping = aes(x = field_x, y = field_y, fill = probability)
  ) +
  geom_point(
    data = mobest::create_spatpos(id = "pioneer", x = 0.75, y = 0.25, z = 1),
    mapping = aes(x = x, y = y),
    colour = "red"
  ) +
  geom_point(
    data = ovs_test,
    mapping = aes(x = field_x, y = field_y),
    colour = "orange"
  ) +
  coord_fixed()
