library(magrittr)
library(ggplot2)

#### data ####

# curves
load("data/poseidon_data/janno_final.RData")
load("data/origin_search/origin_grid_median_modified.RData")
load("data/origin_search/moving_origin_grid.RData")
load("data/origin_search/mean_origin.RData")
load("data/origin_search/no_data_windows.RData")

# maps
load("data/spatial/mobility_regions.RData")
load("data/spatial/research_area.RData")
load("data/spatial/extended_area.RData")
load("data/spatial/epsg102013.RData")
load("data/plot_reference_data/region_id_shapes.RData")

# filter for central europe

purrr::walk(
  janno_final %$% region_id %>% unique,
  function(x) {
  
    print(x)
    
    cur_janno_final <- janno_final[janno_final$region_id == x, ]
    cur_origin_grid_median_modified <- origin_grid_median_modified %>% dplyr::filter(region_id == x)
    cur_moving_origin_grid <- moving_origin_grid %>% dplyr::filter(region_id == x)
    cur_no_data_windows <- no_data_windows %>% dplyr::filter(region_id == x)
    
    # hack to keep the NA in the legend
    cur_origin_grid_median_modified <- rbind(cur_origin_grid_median_modified, rep(NA, 22)) 
    
    #### mobility estimator curves ####
    
    p_estimator <- ggplot() +
      ggpointgrid::geom_pointgrid(
        data = cur_origin_grid_median_modified,
        mapping = aes(
          x = search_z, y = spatial_distance, color = angle_deg,
          shape = origin_region_id
        ),
        size = 1.5,
        grid_x = 100,
        grid_y = 60
      ) +
      scale_shape_manual(
        values = region_id_shapes,
        na.value = 4,
        drop = FALSE
      ) +
      geom_rect(
        data = cur_no_data_windows,
        mapping = aes(
          ymax = Inf,
          ymin = -Inf,
          xmin = min_date_not_covered,
          xmax = max_date_not_covered
        ),
        fill = "lightgrey",
        alpha = 0.7
      ) +
      geom_ribbon(
        data = cur_moving_origin_grid,
        mapping = aes(
          x = z,
          ymin = undirected_mean_spatial_distance - 2*std_spatial_distance,
          ymax = undirected_mean_spatial_distance + 2*std_spatial_distance
        ),
        fill = "lightgrey",
        alpha = 0.7
      ) +
      geom_line(
        data = cur_moving_origin_grid,
        mapping = aes(x = z, y = undirected_mean_spatial_distance, color = mean_angle_deg),
        size = 0.8
      ) +
      geom_rect(
        data = tibble::tibble(xmin = -Inf, ymin = -Inf, ymax = 0, xmax = Inf),
        mapping = aes(
          xmin = xmin, xmax = xmax,
          ymin = ymin, ymax = ymax
        ),
        fill = "white"
      ) +
      geom_point(
        data = cur_janno_final,
        aes(x = Date_BC_AD_Median_Derived, y = -50),
        shape = "|",
        size = 2
      ) +
      theme_bw() +
      theme(
        legend.position = "right",
        legend.background = element_blank(),
        legend.title = element_text(size = 13),
        legend.spacing.y = unit(0.2, 'cm'),
        legend.key.height = unit(0.4, 'cm'),
        legend.text = element_text(size = 10),
        legend.box.margin = margin(50,0,0,0)
      ) +
      guides(
        shape = guide_legend(title = "Region", ncol = 1, byrow = F, override.aes = list(size = 3))
      ) +
      xlab("time in years calBC/calAD") +
      ylab("spatial distance to \"origin\" (undirected mean) [km]") +
      scale_color_gradientn(
        colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"),
        na.value = NA,
        guide = F
      ) +
      scale_x_continuous(breaks = seq(-7000, 1000, 1000)) +
      coord_cartesian(
        ylim = c(0, max(cur_origin_grid_median_modified$spatial_distance)),
        #ylim = c(0, max(origin_grid$spatial_distance, na.rm = T)),
        xlim = c(-7500, 1500)
      )
    
    #### direction legend ####
    
    p_legend <- tibble::tibble(
      ID = letters[1:8],
      angle_start = seq(0, 325, 45),
      angle_stop = seq(45, 360, 45)
    ) %>%
      ggplot() + 
      geom_rect(
        aes(xmin = 3, xmax = 4, ymin = angle_start, ymax = angle_stop, fill = ID)
      ) +
      scale_fill_manual(
        values = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"), 
        guide = FALSE
      ) +
      coord_polar(theta = "y") +
      xlim(2, 4.5) +
      scale_y_continuous(
        breaks = c(0, 45, 90, 135, 180, 225, 270, 315),
        labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10)
      )
    
    #### compile plots ####
    
    p <- cowplot::ggdraw(p_estimator) +
      cowplot::draw_plot(
        p_legend, .76, .69, .3, .3
      )
    
    ggsave(
      paste0(paste0("plots/figure_sup_19_", gsub(" ", "_", as.character(x)) ,".png")),
      plot = p,
      device = "png",
      scale = 0.35,
      dpi = 500,
      width = 850, height = 400, units = "mm",
      limitsize = F
    )
  
  }
)

