library(magrittr)

load("data/simulation/mock_data.RData")

#### interpolation test run ####

interpol_test_res <- mobest::create_model_grid(
  independent = mobest::create_spatpos_multi(ind = independent_list_II[[2]][[6]]),
  dependent = dependent_list_III[[2]][[6]],
  kernel = mobest::create_kernset_multi(kern = kernels_list$kernel_1),
  prediction_grid = mobest::create_spatpos_multi(
    A = mobest::create_geopos(id = 1, x = 0.125, y = 0.875) %>%
        mobest::geopos_to_spatpos(z = seq(0,1,0.05)),
    B = mobest::create_geopos(id = 2, x = 0.875, y = 0.125) %>%
      mobest::geopos_to_spatpos(z = seq(0,1,0.05))
  )
) %>% mobest::run_model_grid()

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

#### store results ####

save(
  interpol_test_res, locate_test_product, ovs,
  file = "data/simulation/example_run.RData"
)
