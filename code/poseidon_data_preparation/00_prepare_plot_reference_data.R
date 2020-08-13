region_id_colors <- c(
  "Central Europe" = "#999999", 
  "Iberia" = "#E69F00", 
  "Eastern Europe" = "#56B4E9", 
  "Britain and Ireland" = "#009E73", 
  "Turkey" = "#871200",
  "France" = "#F0E442", 
  "Near East" = "#0072B2", 
  "Caucasus" = "#D55E00", 
  "Italy" = "#CC79A7", 
  "Southeastern Europe" = "#2fff00"
)
save(region_id_colors, file = "data/plot_reference_data/region_id_colors.RData")

age_group_id_shapes <- c(
  ">-8000" = 0,
  "-8000 - -7000" = 20,
  "-7000 - -6000" = 19,
  "-6000 - -5000" = 18,
  "-5000 - -4000" = 15,
  "-4000 - -3000" = 17,
  "-3000 - -2000" = 1,
  "-2000 - -1000" = 2,
  "-1000 - 0" = 13,
  "0 - 1000" = 3,
  "1000 - 2000" = 4
)
save(age_group_id_shapes, file = "data/plot_reference_data/age_group_id_shapes.RData")
