#### run origin search ####

locate_res <- purrr::pmap_dfr(
  list(pop_sizes, independent_list_II, dependent_list_III),
  function(pop_size, independent_list_I, dependent_list_II) {
    message("Running: ", pop_size, " of ", paste(pop_sizes, collapse = ", "))
    purrr::pmap_dfr(
      list(its, independent_list_I, dependent_list_II), function(it_num, ind, dep) {
        i <- paste0("iteration_", it_num)
        message("Running: ", it_num, " of ", nr_iterations)
        locate_res_single <- mobest::locate_multi(
          independent = mobest::create_spatpos_multi(ind, .names = i),
          dependent = dep,
          kernel = kernels_list,
          search_independent = mobest::create_spatpos_multi(
            mobest::create_spatpos(id = "pioneer", x = 0.75, y = 0.25, z = 1),
            .names = i
          ),
          search_dependent = mobest::create_obs_multi(
            limited_slow = mobest::create_obs(component = 0.25),
            limited_fast = mobest::create_obs(component = 0.25),
            intertwined = mobest::create_obs(component = 0.25)
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
        locate_res_single$pop_size <- pop_size
        return(locate_res_single)
      }
    )
  }
)

locate_product <- mobest::multiply_dependent_probabilities(locate_res)
ovs <- mobest::determine_origin_vectors(
  locate_product, pop_size, independent_table_id, dependent_setting_id, kernel_setting_id, field_z
)

ovs %>%
  dplyr::mutate(
    top_left = (field_x <= 0.5) & (field_y >= 0.5)
  ) %>%
  dplyr::group_by(pop_size, kernel_setting_id, dependent_setting_id, field_z) %>%
  dplyr::summarise(
    n_top_left = sum(top_left),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    kernel_table,
    by = "kernel_setting_id"
  ) %>%
  dplyr::mutate(
    dependent_setting_id = factor(dependent_setting_id, c("limited_slow", "limited_fast", "intertwined"))
  )

