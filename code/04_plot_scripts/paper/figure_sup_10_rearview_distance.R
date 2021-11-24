library(ggplot2)
library(magrittr)

load("data/origin_search/kernel_theta_data.RData")
ktd_mds2 <- kernel_theta_data %>% dplyr::mutate(run = "2D MDS")
load("data/origin_search/kernel_theta_data_mds3.RData")
ktd_mds3 <- kernel_theta_data %>% dplyr::mutate(run = "3D MDS")

get_dist <- function(x, ktd) {
  ktd %>%
    dplyr::filter(k < x) %>%
    head(1) %$% dist_p1_p2
}

settings <- dplyr::bind_rows(
    tibble::tibble(
      run = "2D MDS",
      k = c(0.25, 0.5, 0.75),
      kernel_width = purrr::map_dbl(k, get_dist, ktd_mds2),
      label = latex2exp::TeX(
        paste0("K = ", k, ",$\\sqrt{\\theta}$ = ", kernel_width)
      )
    ),
    tibble::tibble(
      run = "3D MDS",
      k = c(0.5),
      kernel_width = purrr::map_dbl(k, get_dist, ktd_mds3),
      label = latex2exp::TeX(
        paste0("K = ", k, ",$\\sqrt{\\theta}$ = ", kernel_width)
      )
    )
  )

p <- ggplot() +
  geom_line(
    data = dplyr::bind_rows(ktd_mds2, ktd_mds3),
    mapping = aes(dist_p1_p2, k, colour = run),
    size = 1
  ) +
  geom_vline(
    xintercept = settings$kernel_width
  ) +
  geom_point(
    data = settings,
    mapping = aes(x = kernel_width, y = k),
    shape = 4,
    size = 5,
    stroke = 1.5,
    colour = "black"
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
  theme(
    legend.position = "bottom"
  ) +
  xlab("pairwise distance [years]") +
  ylab("Covariance K")

ggsave(
  "plots/figure_sup_10_rearview_distance.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.7,
  dpi = 300,
  width = 300, height = 180, units = "mm",
  limitsize = F
)
