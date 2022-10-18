library(magrittr)

load("data/parameter_exploration/crossvalidation_best_kernels.RData")

parameter_table <- crossvalidation_best_kernels %>%
  dplyr::filter(
    multivar_fstate == "u",
    dplyr::case_when(
      multivar_method == "mds" ~ dim %in% c("C1", "C2"),
      multivar_method == "pca_proj" ~ dim %in% c("C1", "C2", "C3", "C4", "C5"),
      TRUE ~ FALSE
    )
  ) %>%
  dplyr::arrange(multivar_method, dim) %>%
  dplyr::mutate(
    multivar_method = dplyr::recode(
      multivar_method,
      mds = "MDS2",
      pca_proj = "PCA5"
    )
  ) %>%
  dplyr::select(
    `Multivariate method` = multivar_method,
    `Dimension` = dim,
    dsx, dsy, dt, g
  )

parameter_table %>% knitr::kable(format = "latex")
