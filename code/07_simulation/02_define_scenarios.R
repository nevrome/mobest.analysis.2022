linear <- function(x) { x }
limited_slow <- function(x) { 1 - exp(-2*x) }
limited_fast <- function(x) { 1 - exp(-7*x) }
intertwined <- function(x) { x + sin(5*pi * x) }
sd_val <- 0.1

huhu <- purrr::imap_dfr(
 c(intertwined = intertwined, limited_slow = limited_slow, limited_fast = limited_fast),
 function(f, name) {
   dplyr::bind_rows(
    tibble::tibble(
      x      = seq(0,1,0.01),
      y_min  = 0.25 + 0.25 * f(x) - sd_val,
      y_mean = 0.25 + 0.25 * f(x),
      y_max  = 0.25 + 0.25 * f(x) + sd_val,
      group = "A"
    ),
    tibble::tibble(
      x      = seq(0,1,0.01),
      y_min  = 0.75 - 0.25 * f(x) - sd_val,
      y_mean = 0.75 - 0.25 * f(x),
      y_max  = 0.75 - 0.25 * f(x) + sd_val,
      group = "B"
    )
  ) %>% dplyr::mutate(func = name)
})

huhu %>%
  ggplot() +
  facet_wrap(~func) +
  geom_ribbon(aes(x, ymin = y_min, ymax = y_max, fill = group), alpha = 0.2) +
  geom_line(aes(x, y_mean, color = group))
