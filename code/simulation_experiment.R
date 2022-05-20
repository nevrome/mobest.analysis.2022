set.seed(100)

independent <- mobest::create_spatpos(
  id = c(paste0("A", 1:50), paste0("B", 1:50)),
  x = c(runif(50, 0, 0.4), runif(50, 0.6, 1)),
  y = c(runif(100, 0, 1)),
  z = c(runif(100, 0, 1))
)

dependent = mobest::create_obs(
  component = c(
    rnorm(50, 0.2, 0.1) + 0.3 * independent$z[1:50],
    rnorm(50, 0.8, 0.1) - 0.3 * independent$z[51:100]
  )
)

library(magrittr)
library(ggplot2)
ggplot() +
  geom_point(
    data = independent,
    mapping = aes(x, y, color = grepl("A", id))
  )

ggplot() +
  geom_point(
    data = independent,
    mapping = aes(x, z, color = grepl("A", id))
  )


ggplot() +
  geom_point(
    data = dplyr::bind_cols(
      independent,
      dependent
    ),
    mapping = aes(x, y, color = component)
  ) +
  scale_color_viridis_c()

ggplot() +
  geom_point(
    data = dplyr::bind_cols(
      independent,
      dependent
    ),
    mapping = aes(z, component, color = grepl("A", id))
  )

kernel_length <- 0.5

locate_simple <- mobest::locate(
  independent = independent,
  dependent = dependent,
  kernel = mobest::create_kernset(
    component = mobest::create_kernel(
      kernel_length,
      kernel_length,
      kernel_length, 0.1, on_residuals = T)
  ),
  search_independent = mobest::create_spatpos(
    id = "APioneer", x = 0.75, y = 0.5, z = 1
  ),
  search_dependent = mobest::create_obs(
    component = 0.25
  ),
  # spatial search grid: Where to search
  search_space_grid = expand.grid(
    x = seq(0, 1, 0.05), 
    y = seq(0, 1, 0.05)
  ) %>% { mobest::create_geopos(id = 1:nrow(.), x = .$x, y = .$y) },
  # search time: When to search
  search_time = seq(0,1,0.1),
  search_time_mode = "absolute",
  quiet = F
)

hu <- mobest::determine_origin_vectors(mobest::multiply_dependent_probabilities(locate_simple), field_z)

# plot the resulting probability surface
library(ggplot2)
ggplot() +
  facet_wrap(~field_z) +
  geom_raster(
    data = locate_simple,
    mapping = aes(x = field_x, y = field_y, fill = probability)
  ) +
  geom_point(
    data = locate_simple,
    mapping = aes(x = search_x, y = search_y),
    colour = "red"
  ) +
  geom_point(
    data = hu,
    mapping = aes(x = field_x, y = field_y),
    colour = "orange"
  ) +
  coord_fixed()


