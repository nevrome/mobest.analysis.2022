geo_scaled <- d_all$geo_dist/max(d_all$geo_dist)
time_scaled <- d_all$time_dist/max(d_all$time_dist)

d <- function(a, b, c = 0) {
  sqrt(a^2 + b^2 + c^2)
}

gdist3 <- Map(d, d_all$C1_dist, d_all$C2_dist, d_all$C3_dist) %>% unlist()
gdist2 <- Map(d, d_all$C1_dist, d_all$C2_dist) %>% unlist()
stdist <- Map(d, geo_scaled, time_scaled) %>% unlist()

distances <- tibble::tibble(
  stdist = stdist,
  gdist2 = gdist2,
  gdist3 = gdist3
) %>%
  tidyr::pivot_longer(cols = c("gdist2", "gdist3"), names_to = "genetic_distance_method", values_to = "gdist")

r2s <- tibble::tibble(
  genetic_distance_method = c("gdist2", "gdist3"),
  r2 = c(cor(spdist, gdist2)^2, cor(spdist, gdist3)^2)
)

ggplot() +
  geom_hex(
    data = distances,
    mapping = aes(x = stdist, y = gdist, col=..count..)
  ) +
  geom_text(
    data = r2s,
    mapping = aes(x = Inf, y = -Inf, label = paste("R2=", round(r2, 3))),
    hjust = "inward", vjust = "inward"
  ) +
  facet_wrap(~genetic_distance_method) +
  scale_color_gradient(low = "white", high = "black") +
  scale_fill_gradient(low = "white", high = "black")