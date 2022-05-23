library(magrittr)
library(ggplot2)

#### set parameters ####

set.seed(100)
nr_iterations <- 100
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

# x <- seq(0,1,0.01)
# plot(x, x, ylim = c(0,1))
# plot(x, 1-exp(-3*x), ylim = c(0,1))
# plot(x, 1-exp(-7*x), ylim = c(0,1))

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
              rnorm(nrow(igA), 0.25, 0.1) + 0.25 * igA$z,
              rnorm(nrow(igB), 0.75, 0.1) - 0.25 * igB$z
            )
          ),
          limited_slow = mobest::create_obs(
            component = c(
              rnorm(nrow(igA), 0.25, 0.1) + 0.25 * (1 - exp(-3*igA$z)),
              rnorm(nrow(igB), 0.75, 0.1) - 0.25 * (1 - exp(-3*igB$z))
            )
          ),
          limited_fast = mobest::create_obs(
            component = c(
              rnorm(nrow(igA), 0.25, 0.1) + 0.25 * (1 - exp(-7*igA$z)),
              rnorm(nrow(igB), 0.75, 0.1) - 0.25 * (1 - exp(-7*igB$z))
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

#### test/examples ####

overview <- purrr::map2_dfr(
  independent_list_II, dependent_list_III, function(ind_I, dep_II) {
    purrr::map2_dfr(
      ind_I, dep_II, function(ind, dep_I) {
        dep_I_merged <- purrr::imap_dfr(
          dep_I, function(dep, n) {
            dep %>% dplyr::mutate(
              id = seq_len(dplyr::n()),
              process = factor(n, levels = c("linear", "limited_slow", "limited_fast"))
            )
          }
        )
        dplyr::left_join(ind, dep_I_merged, by = "id")
      }  
    )
  }
)

ex1 <- overview %>% dplyr::filter(pop_size == 25, iteration == 6, process == "linear")

ggplot() +
  geom_point(
    data = ex1,
    mapping = aes(x, y, color = group)
  )

ggplot() +
  geom_point(
    data = ex1,
    mapping = aes(x, z, color = group)
  )

ggplot() +
  geom_point(
    data = ex1,
    mapping = aes(z, component, color = group)
  )

ex2 <- overview %>% dplyr::filter(iteration == 6)

ex2 %>%
  ggplot() +
  geom_point(
    mapping = aes(z, component, color = group)
  ) +
  geom_smooth(
    mapping = aes(z, component, color = group)
  ) +
  facet_grid(rows = dplyr::vars(process), cols = dplyr::vars(pop_size))

#### search test run ####

locate_test_res <- mobest::locate(
  independent = independent_list_II[[2]][[6]],
  dependent = dependent_list_III[[2]][[6]]$limited_slow,
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

locate_test_run %>%
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
            linear = mobest::create_obs(component = 0.25), 
            limited_slow = mobest::create_obs(component = 0.25),
            limited_fast = mobest::create_obs(component = 0.25)
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
    dependent_setting_id = factor(dependent_setting_id, c("linear", "limited_slow", "limited_fast"))
  ) %>%
  ggplot() +
  ggh4x::facet_nested(kernel_length ~ pop_size + dependent_setting_id) +
  geom_line(aes(x = field_z, y = n_top_left)) +
  geom_point(aes(x = field_z, y = n_top_left))+ 
  geom_hline(yintercept = nr_iterations/4) +
  scale_x_continuous(breaks = seq(0,1,0.2)) +
  scale_y_continuous(breaks = seq(25, 100, 25)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_cartesian(ylim = c(25,100))

