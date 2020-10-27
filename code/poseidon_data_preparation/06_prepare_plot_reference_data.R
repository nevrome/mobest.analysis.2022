# region colors
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

# age group shapes
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

# spatiotemporal no-data windows
library(magrittr)

load("data/poseidon_data/janno_final.RData")

split_vector_at_na <- function( x ){
  idx <- 1 + cumsum( is.na( x ) )
  not.na <- ! is.na( x )
  split( x[not.na], idx[not.na] )
}

no_data_windows_yearwise <- janno_final %>%
  dplyr::group_by(region_id) %>%
  dplyr::summarise(
    date_not_covered = 
      {
        not_covered <- setdiff(
          -7500:1500,
          lapply(Date_BC_AD_Median_Derived, function(x) {
            seq(x, x+500, 1)
          }) %>% Reduce(union, .)
        )
        schu <- rep(NA, length(-7500:1500))
        schu[-7500:1500 %in% not_covered] <- not_covered
        schu
      }
  )  %>%
  dplyr::ungroup()

save(no_data_windows_yearwise, file = "data/plot_reference_data/no_data_windows_yearwise.RData")

no_data_windows <- no_data_windows_yearwise %>%
  dplyr::group_by(region_id) %>%
  dplyr::summarise(
    min_date_not_covered = sapply(split_vector_at_na(date_not_covered), min),
    max_date_not_covered = sapply(split_vector_at_na(date_not_covered), max)
  ) %>%
  dplyr::ungroup()

save(no_data_windows, file = "data/plot_reference_data/no_data_windows.RData")
