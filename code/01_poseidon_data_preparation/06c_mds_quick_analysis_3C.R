library(magrittr)
library(ggplot2)

mds <- readr::read_delim("data/poseidon_data/mds/poseidon_extracted3.pruned.mds", " ", trim_ws = T) %>%
  dplyr::select(-X7)

janno <- poseidonR::read_janno("data/poseidon_data/poseidon_extracted/poseidon_extracted.janno")

load("data/spatial/mobility_regions.RData")
load("data/spatial/epsg3035.RData")

janno_mds <- janno %>% 
  dplyr::left_join(
    mds, by = c("Poseidon_ID" = "IID")
  )

fig <- plotly::plot_ly(janno_mds, x = ~C1, y = ~C2, z = ~C3, color = ~Date_BC_AD_Median, marker = list(size = 2))
fig <- fig %>% plotly::add_markers(
    text = ~Country
)

ord <- findInterval(janno_mds$Date_BC_AD_Median, sort(janno_mds$Date_BC_AD_Median))
rgl::points3d(
  janno_mds$C1,
  janno_mds$C2,
  janno_mds$C3,
  heat.colors(nrow(janno_mds))[ord]
)

ggplot() +
  geom_point(
    data = janno_mds,
    aes(x = C1, y = C2, color = Date_BC_AD_Median),
    alpha = 0.7,
    size = 2
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "right"
  ) +
  coord_fixed() +
  scale_y_reverse()

