set.seed(100)

independent <- mobest::create_spatpos(
  id = c(paste0("A", 1:50), paste0("B", 1:50)),
  x = c(runif(50, 0, 0.4), runif(50, 0.6, 1)),
  y = c(runif(100, 0, 1)),
  z = c(runif(100, 0, 1))
)

dependent = mobest::create_obs(
  component = purrr::pmap_dbl(
    independent, function(...) {
      row <- list(...)
      if (grepl("A", row$id)) {
        0.25 + sample(c(0.3, -0.3), 1) * sqrt((0.2 - row$x)^2 + (0.5 - row$y)^2) + 0.25 * row$z
      } else {
        0.75 + sample(c(0.3, -0.3), 1) * sqrt((0.8 - row$x)^2 + (0.5 - row$y)^2) - 0.25 * row$z
      }
    }
  )
)

independent

# library(magrittr)
# library(ggplot2)
# ggplot() +
#   geom_point(
#     data = independent,
#     mapping = aes(x, y, color = grepl("A", id))
#   )
# 
# ggplot() +
#   geom_point(
#     data = independent,
#     mapping = aes(x, z, color = grepl("A", id))
#   )
# 

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



locate_simple <- mobest::locate(
  independent = independent,
  dependent = dependent,
  kernel = mobest::create_kernset(
    component = mobest::create_kernel(0.3, 0.3, 0.3, 0.1, on_residuals = T)
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

# plot the resulting probability surface
library(ggplot2)
locate_simple %>%
  #dplyr::filter(field_z < 0.7 & field_z < 0.9) %>%
  ggplot() +
  facet_wrap(~field_z) +
  geom_raster(mapping = aes(x = field_x, y = field_y, fill = probability)) +
  geom_point(mapping = aes(x = search_x, y = search_y), colour = "red") +
  coord_fixed()

