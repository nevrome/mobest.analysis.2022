library(magrittr)

load("data/simulation/mock_data.RData")
load("data/simulation/scenarios.RData")

#### set parameters ####

ex_pop_size_id <- 2
ex_iteration <- 6

#### interpolation test run ####

interpol_test_res <- mobest::create_model_grid(
  independent = mobest::create_spatpos_multi(ind = independent_list_II[[ex_pop_size_id]][[ex_iteration]]),
  dependent = dependent_list_III[[ex_pop_size_id]][[ex_iteration]],
  kernel = kernels_list,
    # mobest::create_kernset_multi(
    #   #kern = kernels_list$kernel_1
    #   limited = mobest::create_kernset(
    #     component = mobest::create_kernel(0.2, 0.2, 0.2, 0.1, on_residuals = T)
    #   )
    # ),
  prediction_grid = mobest::create_spatpos_multi(
    A = mobest::create_geopos(id = 1, x = 0.25, y = 0.75) %>%
        mobest::geopos_to_spatpos(z = seq(0,1,0.05)),
    B = mobest::create_geopos(id = 2, x = 0.75, y = 0.25) %>%
      mobest::geopos_to_spatpos(z = seq(0,1,0.05))
  )
) %>% mobest::run_model_grid() %>%
  dplyr::mutate(
    dependent_setting_id = factor(
      dependent_setting_id,
      levels = c("linear", "limited", "intertwined")
    )
  )

#### search test run ####
search_times <- seq(0.1, 0.9, 0.1)

locate_test_res <- mobest::locate_multi(
  independent = mobest::create_spatpos_multi(A = independent_list_II[[ex_pop_size_id]][[ex_iteration]]),
  dependent = dependent_list_III[[ex_pop_size_id]][[ex_iteration]],
  kernel = mobest::create_kernset_multi(A = kernels_list$kernel_3),
  search_independent = mobest::create_spatpos_multi(
    A = mobest::create_spatpos(
      id = paste0("pioneer_", seq_along(search_times)),
      x = rep(0.75, length(search_times)),
      y = rep(0.25, length(search_times)),
      z = search_times
    )
  ),
  search_dependent = mobest::create_obs_multi(
    linear =      mobest::create_obs(component = 0.2 + 0.3 * linear(search_times)),
    limited =     mobest::create_obs(component = 0.2 + 0.3 * limited(search_times)),
    intertwined = mobest::create_obs(component = 0.2 + 0.3 * intertwined(search_times))
  ),
  # spatial search grid: Where to search
  search_space_grid = expand.grid(
    x = seq(0, 1, 0.05), 
    y = seq(0, 1, 0.05)
  ) %>% { mobest::create_geopos(id = 1:nrow(.), x = .$x, y = .$y) },
  # search time: When to search
  search_time = 0,
  search_time_mode = "relative",
  quiet = T
) %>%
  dplyr::mutate(
    dependent_setting_id = factor(
      dependent_setting_id,
      levels = c("linear", "limited", "intertwined")
    )
  )

locate_test_product <- mobest::multiply_dependent_probabilities(locate_test_res)
ovs <- mobest::determine_origin_vectors(locate_test_product, dependent_setting_id, field_z)

#### store results ####

save(
  interpol_test_res, locate_test_product, ovs,
  file = "data/simulation/example_run.RData"
)
