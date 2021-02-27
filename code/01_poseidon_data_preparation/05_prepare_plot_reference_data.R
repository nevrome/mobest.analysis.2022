# region shapes
region_id_shapes <- c(
  "Britain and Ireland" = 0,
  "France" = 1,
  "Iberia" = 2,
  "Southern Scandinavia" = 15,
  "Central Europe" = 6,
  "Italy" = 5,
  "Baltics" = 16,
  "Eastern Europe" = 17,
  "Southeastern Europe" = 18,
  "Caucasus" = 7,
  "Turkey" = 3,
  "Levant" = 8
)

save(region_id_shapes, file = "data/plot_reference_data/region_id_shapes.RData")

# age group colors
age_colors_gradient <- ggplot2::scale_color_gradient2(
  limits = c(-8000, 2000), low = "black", mid = "#ff4903", high = "#44d62d", midpoint = -5000,
  breaks = seq(-7500, 1500, 1000)
)

save(age_colors_gradient, file = "data/plot_reference_data/age_colors_gradient.RData")
