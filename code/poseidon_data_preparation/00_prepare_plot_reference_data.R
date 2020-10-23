region_id_colors <- c(
  "Britain and Ireland" = "#b8434e", 
  "France" = "#f1B6da", 
  "Iberia" = "#7c7120", 
  "Southern Scandinavia" = "#404040",
  "Central Europe" = "#b86738",
  "Italy" = "#09ebe0", 
  "Baltics" = "#4bc490",
  "Eastern Europe" = "#bfa33a", 
  "Southeastern Europe" = "#6678dc",
  "Caucasus" = "#729d46", 
  "Turkey" = "#5b388a",
  "Levant" = "#b1467b" 
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
