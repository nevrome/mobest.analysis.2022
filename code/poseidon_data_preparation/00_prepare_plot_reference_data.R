region_id_colors <- c(
  "Britain and Ireland" = "#b8434e", 
  "Southern Scandinavia" = "#404040",
  "Baltics" = "#f1B6da",
  "Central Europe" = "#b86738",
  "Eastern Europe" = "#bfa33a", 
  "Caucasus" = "#729d46", 
  "France" = "#4bc490", 
  "Italy" = "#31cdc6", 
  "Southeastern Europe" = "#6678dc",
  "Turkey" = "#5b388a",
  "Iberia" = "#7c7120", 
  "Near East" = "#b1467b" 
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
  "-1000 - 0" = 5,
  "0 - 1000" = 3,
  "1000 - 2000" = 4
)
save(age_group_id_shapes, file = "data/plot_reference_data/age_group_id_shapes.RData")
