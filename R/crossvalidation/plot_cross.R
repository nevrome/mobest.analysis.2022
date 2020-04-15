interpol_comparison_group <- lapply(
  list.files("data/crossvalidation", full.names = T), function(x) {
    load(x)
    interpol_comparison_group
  }
) %>% dplyr::bind_rows()




library(magrittr)
library(ggplot2)

# all PCs
# interpol_comparison_group %>%
#   ggplot() +
#   geom_raster(
#     aes(x = ds, y = dt, fill = sd_difference)
#   ) +
#   scale_fill_viridis_c() +
#   facet_grid(rows = vars(PC), cols = vars(g))


icg <- interpol_comparison_group %>%
  # rank of kernel for each PC within each difference-type
  dplyr::group_by(
    PC
  ) %>%
  dplyr::mutate(
    rank_mean_difference = rank(mean_difference),
    rank_median_difference = rank(median_difference),
    rank_sd_difference = rank(sd_difference),
    rank_diff_5_95_difference = rank(diff_5_95_difference)
  ) %>%
  dplyr::ungroup() %>%
  # median rank of kernel for all PCs and all difference-types
  dplyr::group_by(
    ds, dt, g
  ) %>%
  dplyr::mutate(
    median_rank = median(c(
      rank_mean_difference, 
      rank_median_difference, 
      rank_sd_difference, 
      rank_diff_5_95_difference)
    )
  ) %>%
  dplyr::ungroup()


minicg <- icg %>% dplyr::filter(
  median_rank == min(median_rank)
) %>% dplyr::select(
  ds, dt, g
) %>% unique

ggplot() +
  geom_raster(
    data = icg,
    aes(x = ds, y = dt, fill = median_rank)
  ) +
  scale_fill_viridis_c() +
  facet_wrap(~g) +
  geom_point(
    data = minicg,
    aes(x = ds, y = dt),
    shape = 4,
    color = "red",
    size = 3
  )

###

# ggplot() +
#   geom_histogram(
#     data = interpol_comparison %>% dplyr::filter(!grepl("norm", PC)),
#     mapping = aes(x = difference, fill = kernel_setting_id), bins = 100
#   ) +
#   facet_grid(cols = vars(PC), rows = vars(kernel_setting_id)) +
#   geom_vline(
#     data = interpol_comparison_sd %>% dplyr::filter(!grepl("norm", PC)), 
#     mapping = aes(xintercept = sd_difference),
#     color = "red"
#   ) +
#   geom_vline(
#     data = interpol_comparison_sd %>% dplyr::filter(!grepl("norm", PC)), 
#     mapping = aes(xintercept = 0),
#     color = "black"
#   ) +
#   geom_vline(
#     data = interpol_comparison_sd %>% dplyr::filter(!grepl("norm", PC)), 
#     mapping = aes(xintercept = -sd_difference),
#     color = "red"
#   ) +
#   theme_bw() +
#   xlim(-0.05, 0.05)
# 
# interpol_comparison %>%
#   dplyr::filter(grepl("norm", PC)) %>%
#   ggplot() +
#   geom_histogram(
#     aes(x = difference, fill = kernel_setting_id), bins = 100
#   ) +
#   facet_grid(cols = vars(PC), rows = vars(kernel_setting_id)) +
#   geom_vline(aes(xintercept = 0)) +
#   theme_bw()


  
  
  