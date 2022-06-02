library(magrittr)
library(ggplot2)

load("data/simulation/example_run.RData")

# https://stackoverflow.com/a/61613740/3216883
element_textbox <- function(...) {
  el <- element_text(...)
  class(el) <- c("element_textbox", class(el))
  el
}

element_grob.element_textbox <- function(element, ...) {
  text_grob <- NextMethod()
  rect_grob <- element_grob(calc_element("strip.background", theme_bw()))
  
  ggplot2:::absoluteGrob(
    grid::gList(
      element_grob(calc_element("strip.background", theme_bw())),
      text_grob
    ),
    height = grid::grobHeight(text_grob), 
    width = grid::unit(1, "npc")
  )
}

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
      theme(
        legend.position = "none",
        plot.title = element_textbox(
          hjust = 0.5, margin = margin(t = 5, b = 5), size = 10
        ),
        axis.text = element_text(angle = 45, hjust = 1)
      ) +
      ggtitle(unique(locate_dep$dependent_setting_id))
  }
)

p <- cowplot::plot_grid(
  plotlist = p_list, nrow = 1
)

ggsave(
  paste0("plots/figure_sup_30_simulation_locate_examples.pdf"),
  plot = p,
  device = "pdf",
  scale = 1,
  dpi = 300,
  width = 300, height = 130, units = "mm",
  limitsize = F
)
