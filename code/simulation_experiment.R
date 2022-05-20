library(magrittr)
library(ggplot2)

set.seed(100)

nr_iterations <- 20
its <- seq_len(nr_iterations)

independent <- purrr::map(
  its, function(i) {
    mobest::create_spatpos(
      id = c(paste0("A_", 1:25), paste0("B_", 1:75)),
      x = c(runif(25, 0, 0.5), runif(25, 0.5, 1), runif(25, 0, 0.5), runif(25, 0.5, 1)),
      y = c(runif(25, 0.5, 1), runif(25, 0, 0.5), runif(25, 0, 0.5), runif(25, 0.5, 1)),
      z = c(runif(100, 0, 1))
    )
  }
)

dependent <- purrr::map(
  independent, function(i) {
    mobest::create_obs(
      component = c(
        rnorm(25, 0.25, 0.1) + 0.25 * i$z[1:25],
        rnorm(75, 0.75, 0.1) - 0.25 * i$z[26:100]
      )
    )
  }
)

kernel_table <- tidyr::crossing(
  kernel_length = seq(0.1, 0.5, 0.1),
  nugget = c(0.05, 0.1, 0.2)
) %>%
  dplyr::mutate(
    kernel_setting_id = paste("kernel", 1:nrow(.), sep = "_")
  )

kernels <- purrr::pmap(
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
  magrittr::set_names(kernel_table$kernel_id) %>%
  do.call(mobest::create_kernset_multi, .)

locate_res <- purrr::pmap_dfr(
  list(its, independent, dependent), function(it_num, ind, dep) {
    i <- paste0("iteration_", it_num)
    message("Running: ", it_num, " of ", nr_iterations)
    mobest::locate_multi(
      independent = mobest::create_spatpos_multi(ind, .names = i),
      dependent = mobest::create_obs_multi(dep, .names = i),
      kernel = kernels,
      search_independent = mobest::create_spatpos_multi(
        mobest::create_spatpos(id = "APioneer", x = 0.75, y = 0.25, z = 1),
        .names = i
      ),
      search_dependent = mobest::create_obs_multi(
        mobest::create_obs(component = 0.25), 
        .names = i
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
  }
)

locate_product <- mobest::multiply_dependent_probabilities(locate_res)
ovs <- mobest::determine_origin_vectors(
  locate_product, independent_table_id, dependent_setting_id, kernel_setting_id, field_z
)

ovs %>%
  dplyr::mutate(
    top_left = (field_x <= 0.5) & (field_y >= 0.5)
  ) %>%
  dplyr::group_by(kernel_setting_id, field_z) %>%
  dplyr::summarise(
    n_top_left = sum(top_left),
    sd_top_left = sd(top_left),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    kernel_table,
    by = "kernel_setting_id"
  ) %>%
  ggplot() +
  facet_grid(cols = dplyr::vars(kernel_length)) +
  geom_line(aes(x = field_z, y = n_top_left, color = as.factor(nugget)))# +
  # geom_point(aes(x = field_z, y = n_top_left) ) +
  # geom_errorbar(aes(
  #   x = field_z,
  #   ymin = n_top_left - 2*sd_top_left,
  #   ymax = n_top_left + 2*sd_top_left
  # ))

#### diagnostic plots

ind_group <- independent[[1]] %>% tidyr::separate(id, into = c("group", "id"), sep = "_")
dep1 <- dependent[[1]]
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




