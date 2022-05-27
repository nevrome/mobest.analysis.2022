#### set parameters ####

set.seed(100)
nr_iterations <- 10
its <- seq_len(nr_iterations)
pop_sizes <- c(10, 25, 50)

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
          limited_slow = mobest::create_obs(
            component = c(
              rnorm(nrow(igA), 0.25, 0.1) + 0.25 * limited_slow(igA$z),
              rnorm(nrow(igB), 0.75, 0.1) - 0.25 * limited_slow(igA$z)
            )
          ),
          limited_fast = mobest::create_obs(
            component = c(
              rnorm(nrow(igA), 0.25, 0.1) + 0.25 * limited_fast(igA$z),
              rnorm(nrow(igB), 0.75, 0.1) - 0.25 * limited_fast(igA$z)
            )
          ),
          intertwined = mobest::create_obs(
            component = c(
              rnorm(nrow(igA), 0.25, 0.1) + 0.25 * intertwined(igA$z),
              rnorm(nrow(igB), 0.75, 0.1) - 0.25 * intertwined(igB$z)
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
  kernel_length = seq(0.1, 0.5, 0.1),
  nugget = 0.1,
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
