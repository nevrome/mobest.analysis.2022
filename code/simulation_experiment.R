library(magrittr)
library(ggplot2)

set.seed(100)

independent <- mobest::create_spatpos(
  id = c(paste0("A_", 1:25), paste0("B_", 1:75)),
  x = c(runif(25, 0, 0.5), runif(25, 0.5, 1), runif(25, 0, 0.5), runif(25, 0.5, 1)),
  y = c(runif(25, 0.5, 1), runif(25, 0, 0.5), runif(25, 0, 0.5), runif(25, 0.5, 1)),
  z = c(runif(100, 0, 1))
)

dependent = mobest::create_obs(
  component = c(
    rnorm(25, 0.25, 0.1) + 0.25 * independent$z[1:25],
    rnorm(75, 0.75, 0.1) - 0.25 * independent$z[26:100]
  )
)

ind_group <- independent %>% tidyr::separate(id, into = c("group", "id"), sep = "_")

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

# ggplot() +
#   geom_point(
#     data = dplyr::bind_cols(
#       independent,
#       dependent
#     ),
#     mapping = aes(x, y, color = component)
#   ) +
#   scale_color_viridis_c()

ggplot() +
  geom_point(
    data = dplyr::bind_cols(
      ind_group,
      dependent
    ),
    mapping = aes(z, component, color = group)
  )

kernel_length <- 0.3
nugget <- 0.2

locate_simple <- mobest::locate(
  independent = independent,
  dependent = dependent,
  kernel = mobest::create_kernset(
    component = mobest::create_kernel(
      kernel_length, kernel_length, kernel_length,
      nugget,
      on_residuals = T
    )
  ),
  search_independent = mobest::create_spatpos(
    id = "APioneer", x = 0.75, y = 0.25, z = 1
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


