linear <- function(x) { x }
limited <- function(x) { 1 - exp(-4*x) }
intertwined <- function(x) { 1.5*x + 0.6 * cos(3*pi * (x - 0.6)) }
scenario_sd <- 0.1

scenario_blueprint <- purrr::imap_dfr(
 c(linear = linear, limited = limited, intertwined = intertwined),
 function(f, scenario_name) {
   dplyr::bind_rows(
    tibble::tibble(
      x      = seq(0,1,0.01),
      y_min  = 0.2 + 0.3 * f(x) - scenario_sd,
      y_mean = 0.2 + 0.3 * f(x),
      y_max  = 0.2 + 0.3 * f(x) + scenario_sd,
      group = "A"
    ),
    tibble::tibble(
      x      = seq(0,1,0.01),
      y_min  = 0.8 - 0.3 * f(x) - scenario_sd,
      y_mean = 0.8 - 0.3 * f(x),
      y_max  = 0.8 - 0.3 * f(x) + scenario_sd,
      group = "B"
    )
  ) %>% dplyr::mutate(
    scenario = factor(scenario_name, levels = c("linear", "limited", "intertwined"))
  )
})

# scenario_blueprint %>%
#   ggplot() +
#   facet_wrap(~scenario) +
#   geom_ribbon(aes(x, ymin = y_min, ymax = y_max, fill = group), alpha = 0.2) +
#   geom_line(aes(x, y_mean, color = group)) +
#   theme_bw()

#### store results ####

save(
  linear, limited, intertwined, scenario_sd, scenario_blueprint,
  file = "data/simulation/scenarios.RData"
)
