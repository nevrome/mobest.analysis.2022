# region shapes
region_id_shapes <- c(
  "Britain and Ireland" = 0,
  "Central Europe" = 1,
  "Western Pontic Steppe" = 2,
  "Iberia" = 5,
  "Italy, Sardinia, Adria" = 6,
  "Eastern Balkan" = 13,
  "Eastern Mediterranean" = 10
)

save(region_id_shapes, file = "data/plot_reference_data/region_id_shapes.RData")

# age group colors
age_colors_gradient <- ggplot2::scale_color_gradientn(
  limits = c(-8000, 2000),
  colors = khroma::colour("smooth rainbow", reverse = T)(256, range = c(0, 0.75)),
  breaks = seq(-7500, 1500, 1000)
)

save(age_colors_gradient, file = "data/plot_reference_data/age_colors_gradient.RData")
