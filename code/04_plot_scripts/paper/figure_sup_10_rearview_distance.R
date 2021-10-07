library(ggplot2)
library(magrittr)

load("data/origin_search/kernel_theta_data.RData")

get_dist <- function(x) {
  retrospection_distance_low <- kernel_theta_data %>%
    dplyr::filter(k < x) %>%
    head(1) %$% dist_p1_p2
}

settings <- tibble::tibble(
  k = c(0.25, 0.5, 0.75),
  kernel_width = purrr::map_dbl(k, get_dist),
  label = latex2exp::TeX(
    paste0("K = ", k, ",$\\sqrt{\\theta}$ = ", kernel_width)
  )
)

p <- ggplot() +
  geom_line(
    data = kernel_theta_data,
    mapping = aes(dist_p1_p2, k),
    size = 1
  ) +
  geom_vline(
    xintercept = settings$kernel_width
  ) +
  geom_point(
    data = settings,
    mapping = aes(
      x = kernel_width, y = k
    ),
    shape = 4,
    size = 5,
    stroke = 1.5,
    color = "red"
  ) +
  ggrepel::geom_label_repel(
    data = settings,
    mapping = aes(
      x = kernel_width, y = k, 
      label = label
    ),
    #fill = scales::alpha(c("white"), 0.5),
    point.padding = 15,
    size = 3,
    parse = TRUE
  ) +
  theme_bw() +
  xlab("pairwise distance") +
  ylab("Covariance K")

ggsave(
  "plots/figure_sup_10_rearview_distance.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 150, units = "mm",
  limitsize = F
)
