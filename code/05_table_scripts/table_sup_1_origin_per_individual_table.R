library(magrittr)

load("data/origin_search/origin_grid_modified.RData")
load("data/poseidon_data/janno_final.RData")

cut_angle <- function(x) {
  dplyr::case_when(
    x >= 0   & x <  45  ~ "N",
    x >= 45  & x <  135 ~ "E",
    x >= 135 & x <  225 ~ "S",
    x >= 225 & x <  315 ~ "W",
    x >= 315 & x <= 360 ~ "N",
    TRUE ~ NA_character_
  )
}

origins <- origin_grid_modified %>%
  dplyr::group_by(
    search_id
  ) %>%
  dplyr::summarise(
    origin_centroid_x = mean(origin_x),
    origin_centroid_y = mean(origin_y),
    origin_z = mean(origin_z),
    origin_C1 = mean(origin_mean_C1),
    origin_C2 = mean(origin_mean_C2),
    region_id = dplyr::first(region_id),
    undirected_mean_spatial_distance = mean(spatial_distance),
    directed_mean_spatial_distance = sqrt(
      mean(search_x - origin_x)^2 +
        mean(search_y - origin_y)^2
    ) / 1000,
    mean_angle_deg = mobest::vec2deg(
      c(mean(origin_x - search_x), mean(origin_y - search_y))
    ),
    mean_angle_deg_cut = cut_angle(mean_angle_deg),
    .groups = "drop"
  )

origins_with_janno <- origins %>% dplyr::full_join(
  janno_final %>% dplyr::select(
    Individual_ID, 
    Group_Name, 
    Country, 
    x, 
    y, 
    Date_BC_AD_Median_Derived, 
    C1,
    C2,
    C3
  ),
  by = c("search_id" = "Individual_ID")
) %>%
  dplyr::mutate(
    Group_Name = sapply(Group_Name, function(x) { x[[1]] })
  )
 
origin_table <- origins_with_janno %>%
  dplyr::transmute(
    Individual_ID = search_id,
    Group_Name = Group_Name,
    Country = Country,
    Region = region_id,
    Search_x = x,
    Search_y = y,
    Search_z = Date_BC_AD_Median_Derived,
    Search_C1 = C1,
    Search_C2 = C2,
    Search_C3 = C3,
    Origin_x = origin_centroid_x,
    Origin_y = origin_centroid_y,
    Origin_z = origin_z,
    Origin_C1 = origin_C1,
    Origin_C2 = origin_C2,
    Undirected_mean_spatial_distance = undirected_mean_spatial_distance,
    Directed_mean_spatial_distance = directed_mean_spatial_distance,
    Mean_angle = mean_angle_deg,
    Mean_cardinal_direction = mean_angle_deg_cut
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(Search_C1, Search_C2, Search_C3, Origin_C1, Origin_C2), 
      function(x) { round(x, 4) }
    ),
    dplyr::across(
      c(Search_x, Search_y, Origin_x, Origin_y), 
      function(x) { round(x, -3) }
    ),
    dplyr::across(
      c(Origin_z), 
      function(x) { round(x, -1) }
    ),
    dplyr::across(
      c(Undirected_mean_spatial_distance, Directed_mean_spatial_distance, Mean_angle), 
      as.integer
    )
  )

origin_table %>%
  readr::write_csv(
    "tables/table_sup_1_origin_search_table.csv",
    na = ""
  )

