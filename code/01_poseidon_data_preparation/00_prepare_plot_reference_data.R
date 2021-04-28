# region shapes
region_id_shapes <- c(
  "Iberia" = 0,
  "Britain and Ireland"  = 1,
  "Central Europe" = 2,
  "Eastern Balkan" = 5
)

save(region_id_shapes, file = "data/plot_reference_data/region_id_shapes.RData")

# age group colors
age_colors_gradient <- ggplot2::scale_color_gradientn(
  limits = c(-8000, 2000),
  colors = khroma::colour("smooth rainbow", reverse = T)(256, range = c(0, 0.75)),
  breaks = seq(-7500, 1500, 1000)
)

save(age_colors_gradient, file = "data/plot_reference_data/age_colors_gradient.RData")
