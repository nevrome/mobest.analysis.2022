library(ggplot2)
library(magrittr)

load("data/origin_search/kernel_theta_data.RData")
load("data/origin_search/retrospection_distances.RData")

settings <- dplyr::bind_rows(
    tibble::tibble(
      run = "MDS2",
      ptitle = c("MDS2\\ high-retro", "MDS2\\ default", "MDS2\\ low-retro"),
      k = c(0.2, 0.5, 0.8),
      kernel_width = rev(retrospection_distances),
      label = latex2exp::TeX(
        paste0("$\\overset{K = ", k, ",\\,\\sqrt{\\theta} = ", kernel_width, "}{", ptitle ,"}$")
      )
    ),
    tibble::tibble(
      run = "PCA5",
      ptitle = "PCA5",
      k = c(0.5),
      kernel_width = retrospection_distances[2],
      label = c(latex2exp::TeX(
        paste0("$\\overset{K = ", k, ",\\,\\sqrt{\\theta} = ", kernel_width, "}{", ptitle ,"}$")
      ))
    )
  )

set.seed(127)

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
    mapping = aes(x = kernel_width, y = k),
    shape = 4,
    size = 5,
    stroke = 1.5,
    colour = "red"
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
  ylab("Covariance K") +
  scale_y_continuous(breaks = seq(0,1,0.2))

ggsave(
  "plots/figure_sup_10_rearview_distance2.pdf",
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 300, height = 180, units = "mm",
  limitsize = F
)
