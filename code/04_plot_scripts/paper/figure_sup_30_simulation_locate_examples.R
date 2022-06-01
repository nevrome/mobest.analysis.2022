p_list <- purrr::map2(
  dplyr::group_split(locate_test_product, dependent_setting_id),
  dplyr::group_split(ovs, dependent_setting_id),
  function(locate_dep, ovs_dep) {
    ggplot() +
      facet_wrap(~field_z) +
      geom_raster(
        data = locate_dep,
        mapping = aes(x = field_x, y = field_y, fill = probability)
      ) +
      geom_point(
        data = mobest::create_spatpos(id = "pioneer", x = 0.75, y = 0.25, z = 1),
        mapping = aes(x = x, y = y),
        colour = "red"
      ) +
      geom_point(
        data = ovs_dep,
        mapping = aes(x = field_x, y = field_y),
        colour = "orange"
      ) +
      coord_fixed() +
      theme_bw() +
      theme(legend.position = "none")
  }
)

cowplot::plot_grid(
  plotlist = p_list, nrow = 1
)
