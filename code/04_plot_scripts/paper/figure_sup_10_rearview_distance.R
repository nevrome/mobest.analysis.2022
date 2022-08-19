library(ggplot2)
library(magrittr)

load("data/parameter_exploration/crossvalidation_best_kernels.RData")
load("data/origin_search/retrospection_distances.RData")

crossvalidation_best_kernels %>%
  dplyr::filter(
    dependent_var_id %in% c("C1_mds_u", "C2_mds_u") |
      dependent_var_id %in% c(
        "C1_pca_proj_u",
        "C2_pca_proj_u",
        "C3_pca_proj_u",
        "C4_pca_proj_u",
        "C5_pca_proj_u"
      )
  )

tidyr::crossing()

# WIP

settings <- dplyr::bind_rows(
    tibble::tibble(
      run = "2D MDS",
      ptitle = c("high-retro", "default", "low-retro"),
      k = c(0.25, 0.5, 0.75),
      kernel_width = purrr::map_dbl(k, get_dist, ktd_mds2),
      label = latex2exp::TeX(
        paste0("$\\overset{K = ", k, ",\\,\\sqrt{\\theta} = ", kernel_width, "}{", ptitle ,"}$")
      )
    ),
    tibble::tibble(
      run = "5D projected PCA",
      ptitle = "3D\\,MDS",
      k = c(0.5),
      kernel_width = purrr::map_dbl(k, get_dist, ktd_mds3),
      label = c(latex2exp::TeX(
        paste0("$\\overset{K = ", k, ",\\,\\sqrt{\\theta} = ", kernel_width, "}{", ptitle ,"}$")
      ))
    )
  )

set.seed(127)

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
  "plots/figure_sup_10_rearview_distance.pdf",
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 300, height = 180, units = "mm",
  limitsize = F
)
