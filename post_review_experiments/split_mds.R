library(magrittr)

load("data/poseidon_data/janno_without_identicals.RData")

janno_with_phases <- janno_without_identicals %>%
  dplyr::mutate(
    Phase = purrr::map_chr(
      Date_BC_AD_Prob,
      function(prob_df) {
        prob_filtered <- prob_df %>%
          dplyr::filter(two_sigma)
        start <- min(prob_filtered$age)
        end   <- max(prob_filtered$age)
        dplyr::case_when(
          start < -2800 & end > -3200 ~ "Both Phases",
          start < -2800               ~ "Phase I",
          end   > -3200               ~ "Phase II"
        )
      }
    )
  )

janno_with_phases %>%
  dplyr::filter(Phase %in% c("Both Phases", "Phase II")) %>%
  dplyr::transmute(
    ind = paste0("<", sort(Individual_ID), ">")
  ) %>% 
  readr::write_delim(
    file = "post_review_experiments/data/ind_list_phaseII.txt",
    delim = " ",
    col_names = FALSE
  )

# run: phaseII_mds.sh

read_mds <- function(x) {
  readr::read_fwf(
    file = x, 
    col_positions = readr::fwf_empty(
      x,
      skip = 1,
      col_names = c("FID", "IID", "SOL", paste0("C", 1:10)),
      n = 3000
    ),
    trim_ws = T,
    col_types = "ccdddd_",
    skip = 1
  )
}

mds <- read_mds("post_review_experiments/data/phaseII_package/phaseII.pruned.mds.mds") %>% 
  dplyr::rename(
    Poseidon_ID = IID
  ) %>% dplyr::select(
    -FID, -SOL
  )

janno_mds <- janno_with_phases %>%
  dplyr::left_join(mds, by = c("Individual_ID" = "Poseidon_ID"))

# add spatial and temporal grouping and coordinates
load("data/spatial/mobility_regions.RData")
load("data/spatial/epsg3035.RData")

janno_spatial <- janno_mds %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  sf::st_transform(crs = epsg3035)

region_vector <- janno_spatial %>% 
  sf::st_intersects(mobility_regions, sparse = FALSE) %>%
  as.data.frame() %>%
  magrittr::set_names(as.character(mobility_regions$region_id)) %>%
  dplyr::mutate(id = seq_len(nrow(.))) %>%
  tidyr::pivot_longer(setdiff(everything(), one_of("id")), names_to = "region") %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    region_id = if (any(value)) { region[value] } else { "Other region" }
  ) %$%
  factor(region_id, levels = c(levels(mobility_regions$region_id), "Other region"))

janno_final <- janno_spatial %>%
  dplyr::mutate(
    region_id = region_vector,
    age_group_id = cut(
      Date_BC_AD_Median_Derived, 
      breaks = c(-10000, seq(-8000, 2000, 1000)), 
      labels = c(">-8000", paste0(seq(-8000, 1000, 1000), " - ", seq(-7000, 2000, 1000)))
    ),
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]
  ) %>%
  sf::st_drop_geometry()

# finalize data
janno_final <- janno_final %>% dplyr::arrange(
  Date_BC_AD_Median_Derived
) %>%
  dplyr::mutate(
    pop = purrr::map_chr(Group_Name, function(x) { x[[1]] })
  )

#### plot ####

library(ggplot2)

load("data/plot_reference_data/region_id_shapes.RData")
load("data/plot_reference_data/age_colors_gradient.RData")

p <- ggplot() +
  geom_point(
    data = janno_final,
    aes(
      x = C1, y = C2, 
      color = Date_BC_AD_Median_Derived,
      shape = region_id,
      label = pop
    ),
    size = 2
  ) +
  scale_shape_manual(
    values = region_id_shapes,
    na.value = 3
  ) +
  age_colors_gradient +
  coord_fixed() +
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  scale_x_continuous(breaks = seq(-0.1, 0.1, 0.02)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.background = element_blank(),
    legend.title = element_text(size = 13),
    legend.spacing.y = unit(0.2, 'cm'),
    legend.key.height = unit(0.4, 'cm'),
    legend.text = element_text(size = 10),
  ) +
  guides(
    color = guide_colorbar(title = "Time", barwidth = 20, barheight = 1.5),
    shape = guide_legend(
      title = "Region", nrow = 3, ncol = 3, byrow = T,
      override.aes = aes(size = 3, stroke = 1)
    )
  )

plotly::ggplotly(p)
