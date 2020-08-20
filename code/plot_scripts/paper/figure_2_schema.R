library(magrittr)

DiagrammeR::grViz("code/plot_scripts/figure_2_schema.dot") %>%
  DiagrammeRsvg::export_svg() %>% 
  charToRaw %>% 
  rsvg::rsvg_png(
    "plots/figure_2_schema.png", 
    width = 1000
  )

