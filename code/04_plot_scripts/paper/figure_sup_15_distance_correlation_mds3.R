library(magrittr)
library(ggplot2)

load("data/parameter_exploration/targeted/all_distances.RData")

d <- function(a, b, c = 0) { sqrt(a^2 + b^2 + c^2) }

gdist2 <- Map(d, d_all$C1_mds_u_dist, d_all$C2_mds_u_dist) %>% unlist()
gdist3 <- d_all$obs_dist_total
stdist <- Map(d, d_all$geo_dist, d_all$time_dist) %>% unlist()

distances <- tibble::tibble(
  stdist = stdist,
  gdist2 = gdist2,
  gdist3 = gdist3
) %>%
  tidyr::pivot_longer(
    cols = c("gdist2", "gdist3"), 
    names_to = "genetic_distance_method", 
    values_to = "gdist"
  ) %>% 
  dplyr::mutate(
    genetic_distance_method = dplyr::recode(
      genetic_distance_method,
      "gdist2" = "C1 and C2",
      "gdist3" = "C1, C2 and C3"
    )
  )

r2s <- tibble::tibble(
  genetic_distance_method = c("C1 and C2", "C1, C2 and C3"),
  r2 = c(cor(stdist, gdist2)^2, cor(stdist, gdist3)^2)
)

p <- ggplot(
    #data = distances %>% dplyr::slice_head(n = 1000),
    data = distances,
    mapping = aes(x = stdist, y = gdist)
  ) +
  stat_bin_2d(
    mapping = aes(colour = ..count..)
  ) +
  geom_label(
    data = r2s,
    mapping = aes(x = Inf, y = Inf, label = paste("R^2 ==", round(r2, 3))),
    hjust = "inward", vjust = "inward", label.size = 0, fill = scales::alpha("white", .5),
    parse = T
  ) +
  facet_wrap(~genetic_distance_method) +
  scale_fill_gradient(low = "grey90", high = "black") +
  scale_colour_gradient(low = "grey90", high = "black", guide = "none") +
  theme_bw() +
  xlab("Euclidean distance in space and time (1km=1year)") +
  ylab("Euclidean distance in genetic MDS space")

ggsave(
  "plots/figure_sup_15_distance_correlation_mds32.pdf",
  plot = p,
  device = "pdf",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 170, units = "mm",
  limitsize = F
)
