library(magrittr)
library(ggplot2)

load("data/parameter_exploration/variogram/all_distances.RData")

d <- function(a, b, c = 0) {
  sqrt(a^2 + b^2 + c^2)
}

gdist3 <- d_all$obs_dist_total
gdist2 <- Map(d, d_all$C1_dist, d_all$C2_dist) %>% unlist()
stdist <- Map(d, d_all$geo_dist, d_all$time_dist) %>% unlist()

distances <- tibble::tibble(
  stdist = stdist,
  gdist2 = gdist2,
  gdist3 = gdist3
) %>%
  tidyr::pivot_longer(cols = c("gdist2", "gdist3"), names_to = "genetic_distance_method", values_to = "gdist")

r2s <- tibble::tibble(
  genetic_distance_method = c("gdist2", "gdist3"),
  r2 = c(cor(stdist, gdist2)^2, cor(stdist, gdist3)^2)
)

ggplot() +
  geom_bin_2d(
    data = distances,
    mapping = aes(x = stdist, y = gdist, z=..count..)
  ) +
  geom_text(
    data = r2s,
    mapping = aes(x = Inf, y = -Inf, label = paste("R2=", round(r2, 3))),
    hjust = "inward", vjust = "inward"
  ) +
  facet_wrap(~genetic_distance_method) +
  scale_color_gradient(low = "grey90", high = "black") +
  scale_fill_gradient(low = "grey90", high = "black") +
  theme_bw()
