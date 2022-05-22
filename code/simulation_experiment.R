library(magrittr)
library(ggplot2)

#### set parameters ####

set.seed(100)
nr_iterations <- 20
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
          id = c(paste0("A_", popA), paste0("B_", popB)),
          x = c(runif(popA, 0, 0.5), runif(popA, 0.5, 1), runif(popA, 0, 0.5), runif(popA, 0.5, 1)),
          y = c(runif(popA, 0.5, 1), runif(popA, 0, 0.5), runif(popA, 0, 0.5), runif(popA, 0.5, 1)),
          z = c(runif(pop_size*4, 0, 1))
        )
      }
    )
    return(independent_list_I)
  }
)

# x <- seq(0,1,0.01)
# plot(x, 1-exp(-3*x))

dependent_list_III <- purrr::map(
  independent_list_II, function(independent_list_I) {
    dependent_list_II <-  purrr::map(
      independent_list_I, function(i) {
        dependent_list <- mobest::create_obs_multi(
          linear = mobest::create_obs(
            component = dplyr::case_when(
              grepl("A", i$id) ~ rnorm(1, 0.25, 0.1) + 0.25 * i$z,
              grepl("B", i$id) ~ rnorm(1, 0.75, 0.1) - 0.25 * i$z
            )
          ),
          limited_slow = mobest::create_obs(
            component = dplyr::case_when(
              grepl("A", i$id) ~ rnorm(1, 0.25, 0.1) + 0.25 * (1 - exp(-3*i$z)),
              grepl("B", i$id) ~ rnorm(1, 0.75, 0.1) - 0.25 * (1 - exp(-3*i$z))
            )
          ),
          limited_fast = mobest::create_obs(
            component = dplyr::case_when(
              grepl("A", i$id) ~ rnorm(1, 0.25, 0.1) + 0.25 * (1 - exp(-10*i$z)),
              grepl("B", i$id) ~ rnorm(1, 0.75, 0.1) - 0.25 * (1 - exp(-10*i$z))
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
            x = seq(0, 1, 0.05), 
            y = seq(0, 1, 0.05)
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
  theme_bw() +
  ggh4x::facet_nested(kernel_length ~ pop_size + dependent_setting_id) +
  geom_line(aes(x = field_z, y = n_top_left)) +
  geom_point(aes(x = field_z, y = n_top_left))+ 
  geom_hline(yintercept = nr_iterations/4)# +
  #coord_cartesian(ylim = c(25,100))

#### diagnostic plots

ind_group <- independent_list_I[[1]] %>% tidyr::separate(id, into = c("group", "id"), sep = "_")
dep1 <- dependent_list_II[[1]]$linear

spacetime_and_genetics <- purrr::map2_dfr(
  independent_list_I[1:10], dependent_list_II[1:10], function(ind, dep) {
    dplyr::left_join(
      ind %>% tidyr::separate(id, into = c("group", "id_num"), sep = "_", remove = F),
      tibble::enframe(dep) %>%
        tidyr::unnest(cols = value) %>%
        dplyr::mutate(id = rep(ind$id, length(dep))),
      by = "id"
    )
  }
)

ls1 <- locate_simple %>% dplyr::filter(
  independent_table_id == "iteration_1",
  dependent_setting_id == "iteration_1"
)
ovs1 <- ovs %>% dplyr::filter(
  independent_table_id == "iteration_1",
  dependent_setting_id == "iteration_1"
)

ggplot() +
  geom_point(
    data = ind_group,
    mapping = aes(x, y, color = group)
  )

ggplot() +
  geom_point(
    data = ind_group,
    mapping = aes(x, z, color = group)
  )

ggplot() +
  geom_point(
    data = dplyr::bind_cols(ind_group, dep1),
    mapping = aes(z, component, color = group)
  )

spacetime_and_genetics %>%
ggplot() +
  geom_point(
    mapping = aes(z, component, color = group)
  ) +
  geom_smooth(
    mapping = aes(z, component, color = group)
  ) +
  facet_wrap(~name)

ggplot() +
  facet_wrap(~field_z) +
  geom_raster(
    data = ls1,
    mapping = aes(x = field_x, y = field_y, fill = probability)
  ) +
  geom_point(
    data = ls1,
    mapping = aes(x = search_x, y = search_y),
    colour = "red"
  ) +
  geom_point(
    data = ovs1,
    mapping = aes(x = field_x, y = field_y),
    colour = "orange"
  ) +
  coord_fixed()




