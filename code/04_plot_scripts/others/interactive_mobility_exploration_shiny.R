library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(shiny)

# origin search
load("data/poseidon_data/janno_final.RData")
load("data/origin_search/origin_grid_mean.RData")

origin_grid_mean_infodense <- origin_grid_mean %>%
  dplyr::left_join(
    janno_final %>% dplyr::select(-region_id),
    by = c("search_id" = "Individual_ID")
  ) %>%
  dplyr::mutate(
    Pop = sapply(Group_Name, \(x) x[[1]]),
    Pup = sapply(Publication_Status, \(x) x[[1]])
  ) %>%
  dplyr::select(-Date_BC_AD_Prob) %>%
  dplyr::mutate(
    across(
      where(is.list), 
      function(x) { paste0(x, sep = ",") }
    )
  )


ui <- fluidPage(
  fluidRow(
    column(
      width = 6,
      plotOutput("plot1", height = 800,
          # Equivalent to: click = clickOpts(id = "plot_click")
          click = "plot1_click",
          brush = brushOpts(
            id = "plot1_brush"
          )
       )
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
      lemon::facet_rep_wrap(~region_id, ncol = 2, repeat.tick.labels = T) +
      geom_point(
        data = origin_grid_mean_infodense,
        mapping = aes(
          x = mean_search_z, y = undirected_mean_spatial_distance, color = mean_angle_deg,
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
        colours = c("#F5793A", "#85C0F9", "#85C0F9", "#A95AA1", "#A95AA1", "#33a02c", "#33a02c", "#F5793A"),
        na.value = NA,
        guide = F
      ) +
      scale_x_continuous(breaks = seq(-7000, 1000, 1000)) +
      coord_cartesian(
        xlim = c(-7400, 1400),
        ylim = c(-100, max(origin_grid_mean_infodense$undirected_mean_spatial_distance, na.rm = T))
      )
  })
  
  output$brush_info <- DT::renderDataTable({
    DT::datatable(brushedPoints(origin_grid_mean_infodense, input$plot1_brush))
  })
}

shinyApp(ui, server)