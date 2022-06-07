# qsub -b y -cwd -q archgen.q -pe smp 32 -l h_vmem=10G -now n -V -j y -o ~/log -N simulation singularity exec --bind=/mnt/archgen/users/schmid singularity_mobest.sif Rscript code/07_simulation/05_run_permutations.R

library(magrittr)

load("data/simulation/mock_data.RData")

#### set parameters ####

spatial_search_grid <- seq(0, 1, 0.05)
search_times <- seq(0.1,0.9,0.1)

#### run origin search ####

nr_iterations <- length(independent_list_II[[1]])
its <- seq_len(nr_iterations)

locate_res <- purrr::pmap_dfr(
  list(independent_list_II, dependent_list_III),
  function(independent_list_I, dependent_list_II) {
    pop_size <- unique(independent_list_I[[1]]$pop_size)
    message("Running pop size: ", pop_size)
    purrr::pmap_dfr(
      list(its, independent_list_I, dependent_list_II), function(it_num, ind, dep) {
        i <- paste0("iteration_", it_num)
        message("Running: ", it_num, " of ", nr_iterations)
        locate_res_single <- mobest::locate_multi(
          independent = mobest::create_spatpos_multi(ind, .names = i),
          dependent = dep,
          kernel = kernels_list,
          search_independent = mobest::create_spatpos_multi(
            mobest::create_spatpos(
              id = paste0("pioneer_", seq_along(search_times)),
              x = rep(0.75, length(search_times)),
              y = rep(0.25, length(search_times)),
              z = search_times
            ),
            .names = i
          ),
          search_dependent = mobest::create_obs_multi(
            limited_slow = mobest::create_obs(component = limited_slow(search_times)),
            limited_fast = mobest::create_obs(component = limited_fast(search_times)),
            intertwined  = mobest::create_obs(component = intertwined(search_times))
          ),
          # spatial search grid: Where to search
          search_space_grid = expand.grid(
            x = spatial_search_grid, 
            y = spatial_search_grid
          ) %>% { mobest::create_geopos(id = 1:nrow(.), x = .$x, y = .$y) },
          # search time: When to search
          search_time = 0,
          search_time_mode = "relative",
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

#### summary results in terms of accuracy ####

permutations_accuracy_summary <- ovs %>%
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

#### store results ####

save(
  permutations_accuracy_summary,
  file = "data/simulation/permutations_accuracy_summary.RData"
)
