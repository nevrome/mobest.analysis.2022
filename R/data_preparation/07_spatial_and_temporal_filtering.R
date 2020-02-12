library(magrittr)

# load data
load("data/spatial/research_area.RData")
load("data/anno_1240K_and_anno_1240K_HumanOrigins_simple.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_simple

# spatial transformation and filtering
anno %<>%
  # make sf
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>%
  # transform coordinate system
  sf::st_transform("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs") %>%
  # crop to research area
  sf::st_intersection(research_area) %>%
  # transform back to tibble
  dplyr::mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]
  ) %>%
  tibble::as_tibble() %>%
  dplyr::select(-geometry, -id)

# temporal filtering
anno %<>%
  dplyr::mutate(
    in_time_of_interest = purrr::map(age_prob_distribution_BC, function(x){
          any(x$age >= -10000 & x$age <= 0 )
      }
    )
  ) %>%
  dplyr::filter(
    in_time_of_interest == TRUE
  ) %>%
  dplyr::select(-in_time_of_interest)

# store results
anno_1240K_and_anno_1240K_HumanOrigins_filtered <- anno
save(anno_1240K_and_anno_1240K_HumanOrigins_filtered, file = "data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
