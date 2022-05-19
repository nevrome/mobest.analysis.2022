independent <- mobest::create_spatpos(
  id = c(paste0("A", 1:500), paste0("B", 1:500)),
  x = c(runif(500, 0, 0.5), runif(500, 0.5, 1)), # space x
  y = c(runif(1000, 0, 1)), # space y
  z = c(runif(1000, 0, 1))       # time
)

dependent = mobest::create_obs(
  component = purrr::pmap_dbl(
    independent, function(...) {
      row <- list(...)
      if (grepl("A", row$id)) {
        row$x + 0.25 * row$z
      } else {
        row$x - 0.25 * row$z
      }
    }
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
    mapping = aes(z, component, color = grepl("A", id))
  )

locate_simple <- mobest::locate(
  independent = independent,
  dependent = dependent,
  kernel = mobest::create_kernset(
    component = mobest::create_kernel(0.2, 0.2, 0.2, 0.5, on_residuals = F)
  ),
  search_independent = mobest::create_spatpos(
    id = "APioneer", x = 0.75, y = 0.5, z = 1
  ),
  search_dependent = mobest::create_obs(
    component = 0.25
  ),
  # spatial search grid: Where to search
  search_space_grid = expand.grid(
    x = seq(0, 1, 0.01), 
    y = seq(0, 1, 0.01)
  ) %>% { mobest::create_geopos(id = 1:nrow(.), x = .$x, y = .$y) },
  # search time: When to search
  search_time = seq(0,1,0.2),
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
  coord_fixed() +
  ggtitle(paste0(
    "t for sample of interest = ", unique(locate_simple$search_z), "\n",
    "t field time slice = ", unique(locate_simple$field_z)
  ))

