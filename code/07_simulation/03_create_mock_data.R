load("data/simulation/scenarios.RData")

#### set parameters ####

set.seed(100)
nr_iterations <- 100
its <- seq_len(nr_iterations)
pop_sizes <- c(10, 50, 100)

#### prepare input data ####

independent_list_II <- purrr::map(
  pop_sizes, function(pop_size) {
    popA <- seq_len(pop_size)
    popB <- seq_len(pop_size*3)
    independent_list_I <- purrr::map(
      its, function(i) {
        mobest::create_spatpos(
          id = seq_len(pop_size*4),
          x = c(runif(popA, 0, 0.5), runif(popA, 0.5, 1), runif(popA, 0, 0.5), runif(popA, 0.5, 1)),
          y = c(runif(popA, 0.5, 1), runif(popA, 0, 0.5), runif(popA, 0, 0.5), runif(popA, 0.5, 1)),
          z = c(runif(pop_size*4, 0, 1))
        ) %>%
          dplyr::mutate(
            group = c(rep("A", pop_size), rep("B", pop_size*3)),
            pop_size = pop_size,
            iteration = i
          )
      }
    )
    return(independent_list_I)
  }
)

dependent_list_III <- purrr::map(
  independent_list_II, function(independent_list_I) {
    dependent_list_II <-  purrr::map(
      independent_list_I, function(i) {
        ig <- dplyr::group_split(i, group)
        igA <- ig[[1]]
        igB <- ig[[2]]
        dependent_list <- mobest::create_obs_multi(
          linear = mobest::create_obs(
            component = c(
              rnorm(nrow(igA), 0.2, scenario_sd) + 0.3 * linear(igA$z),
              rnorm(nrow(igB), 0.8, scenario_sd) - 0.3 * linear(igA$z)
            )
          ),
          limited = mobest::create_obs(
            component = c(
              rnorm(nrow(igA), 0.2, scenario_sd) + 0.3 * limited(igA$z),
              rnorm(nrow(igB), 0.8, scenario_sd) - 0.3 * limited(igA$z)
            )
          ),
          intertwined = mobest::create_obs(
            component = c(
              rnorm(nrow(igA), 0.2, scenario_sd) + 0.3 * intertwined(igA$z),
              rnorm(nrow(igB), 0.8, scenario_sd) - 0.3 * intertwined(igB$z)
            )
          )
        )
        return(dependent_list)
      }
    )
    return(dependent_list_II)
  }
)

kernel_table <- tidyr::crossing(
  kernel_length = seq(0.1, 0.5, 0.2),
  nugget = scenario_sd,
) %>%
  dplyr::mutate(
    kernel_setting_id = paste("kernel", 1:nrow(.), sep = "_")
  )

kernels_list <- purrr::pmap(
  kernel_table, function(...) {
    row <- list(...)
    mobest::create_kernset(
      component = mobest::create_kernel(
        row$kernel_length, row$kernel_length, row$kernel_length,
        row$nugget,
        on_residuals = T
      )
    )
  }) %>%
  magrittr::set_names(kernel_table$kernel_setting_id) %>%
  do.call(mobest::create_kernset_multi, .)

#### summarize data ####

mock_data_overview <- purrr::map2_dfr(
  independent_list_II, dependent_list_III, function(ind_I, dep_II) {
    purrr::map2_dfr(
      ind_I, dep_II, function(ind, dep_I) {
        dep_I_merged <- purrr::imap_dfr(
          dep_I, function(dep, n) {
            dep %>% dplyr::mutate(
              id = seq_len(dplyr::n()),
              scenario = factor(n, levels = c("linear", "limited", "intertwined"))
            )
          }
        )
        dplyr::left_join(ind, dep_I_merged, by = "id")
      }  
    )
  }
)

#### store results ####

save(
  independent_list_II, dependent_list_III, kernel_table, kernels_list, mock_data_overview,
  file = "data/simulation/mock_data.RData"
)

