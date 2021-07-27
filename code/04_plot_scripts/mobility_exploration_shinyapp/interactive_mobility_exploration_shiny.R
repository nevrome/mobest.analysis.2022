library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(shiny)
library(tidyverse)
library(sf)
library(magrittr)

# file.copy(
#   from = c(
#     "data/poseidon_data/janno_final.RData",
#     "data/origin_search/origin_grid_mean.RData",
#     "data/origin_search/origin_grid_modified.RData",
#     "data/spatial/extended_area.RData",
#     "data/spatial/epsg3035.RData"
#   ),
#   to = c(
#     "code/04_plot_scripts/mobility_exploration_shinyapp/janno_final.RData",
#     "code/04_plot_scripts/mobility_exploration_shinyapp/origin_grid_mean.RData",
#     "code/04_plot_scripts/mobility_exploration_shinyapp/origin_grid_modified.RData",
#     "code/04_plot_scripts/mobility_exploration_shinyapp/extended_area.RData",
#     "code/04_plot_scripts/mobility_exploration_shinyapp/epsg3035.RData"
#   ),
#   overwrite = T
# )

# origin search
load("janno_final.RData")
load("origin_grid_mean.RData")
load("origin_grid_modified.RData")
load("extended_area.RData")
load("epsg3035.RData")

origin_grid_mean_infodense <- origin_grid_mean %>%
  dplyr::left_join(
    janno_final %>% dplyr::select(-region_id),
    by = c("search_id" = "Individual_ID")
  ) %>%
  tibble::add_column(
    Pop = sapply(.$Group_Name, \(x) x[[1]]),
    Pup = sapply(.$Publication_Status, \(x) x[[1]]),
    .after = "search_id"
  ) %>%
  dplyr::select(-Date_BC_AD_Prob) %>%
  dplyr::select_if(
    tidyselect:::where(\(x) !is.list(x))
  )
  # dplyr::mutate(
  #   across(
  #     where(is.list), 
  #     function(x) { paste0(x, sep = ",") }
  #   )
  # )


ui <- fluidPage(
  fluidRow(
    column(
      width = 12,
      plotOutput("plot1", height = 400,
          # Equivalent to: click = clickOpts(id = "plot_click")
          click = "plot1_click",
          brush = brushOpts(
            id = "plot1_brush"
          )
       )
    )
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput("plot2", height = 800)
    ),
    column(
      width = 6,
      plotOutput("plot3", height = 800)
    )
  ),
  fluidRow(
    column(width = 12,
           h4("Selected points"),
           DT::dataTableOutput("brush_info")
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot() +
      lemon::facet_rep_wrap(~region_id, nrow = 2, repeat.tick.labels = T) +
      geom_point(
        data = origin_grid_mean_infodense,
        mapping = aes(
          x = mean_search_z, y = directed_mean_spatial_distance, color = mean_angle_deg,
          label1 = search_id, label2 = Pop, label3 = Pup
        ),
        alpha = 1,
        size = 1.5,
        shape = 4
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
      ) +
      xlab("time in years calBC/calAD") +
      ylab("spatial distance to \"link point\" (undirected mean) [km]") +
      scale_color_gradientn(
        colours = c("#F5793A", "#85C0F9", "#A95AA1", "#33a02c", "#F5793A"),
        na.value = NA,
        guide = F
      ) +
      scale_x_continuous(breaks = seq(-7000, 1000, 1000)) +
      coord_cartesian(
        xlim = c(-7400, 1400),
        ylim = c(-100, max(origin_grid_mean_infodense$directed_mean_spatial_distance, na.rm = T))
      )
  })
  
  output$brush_info <- DT::renderDataTable({
    DT::datatable(brushedPoints(origin_grid_mean_infodense, input$plot1_brush))
  })
  
  output$plot2 <- renderPlot({
    selected_samples <- brushedPoints(origin_grid_mean_infodense, input$plot1_brush)$search_id
    all_data <- origin_grid_modified %>% 
      dplyr::filter(search_id %in% selected_samples)
    mod_data <- origin_grid_mean_infodense %>% 
      dplyr::filter(search_id %in% selected_samples)
    ggplot() +
      geom_sf(
        data = extended_area,
        fill = "white", colour = "darkgrey", size = 0.4
      ) +
      coord_sf(
        expand = FALSE,
        crs = sf::st_crs(epsg3035)
      ) + 
      geom_jitter(
        data = all_data,
        aes(x = origin_x, y = origin_y),
        color = "orange",
        alpha = 0.2,
        width = 50000, height = 50000
      ) +
      geom_point(
        data = mod_data,
        aes(x = x, y = y),
        color = "red"
      )
  })
  
  output$plot3 <- renderPlot({
    selected_samples <- brushedPoints(origin_grid_mean_infodense, input$plot1_brush)$search_id
    mod_data <- origin_grid_mean_infodense %>% 
      dplyr::filter(search_id %in% selected_samples)
    ggplot() +
      geom_point(
        data = origin_grid_mean_infodense,
        aes(x = C1, y = C2),
        color = "grey"
      ) +
      geom_point(
        data = mod_data,
        aes(x = C1, y = C2),
        color = "red"
      ) +
      coord_fixed()
  })
}

shinyApp(ui, server)