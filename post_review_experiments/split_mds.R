library(magrittr)
library(ggplot2)

#### pca experiment ####

pca_out <- smartsnp::smart_pca(
  "data/poseidon_data/poseidon_extracted/poseidon_extracted.geno",
  sample_group = 1:500,
  missing_impute = "mean"
)

#### experiment with MDS + capture vs. shotgun ####

load("data/poseidon_data/janno_final.RData")

janno_final %>%
  dplyr::mutate(
    Capture_Type = dplyr::case_when(
      grepl(".SG", Poseidon_ID) ~ "Shotgun",
      TRUE ~ "Capture"
    )
  ) %>%
  ggplot() +
  geom_point(
    aes(C1, C3, colour = Capture_Type)
  )
  


#### phase experiment ####

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

janno_phaseII <- janno_with_phases %>%
  dplyr::filter(Phase %in% c("Both Phases", "Phase II"))

janno_phaseII %>%
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

janno_phaseII_mds <- janno_phaseII %>%
  dplyr::left_join(mds, by = c("Individual_ID" = "Poseidon_ID"))

# add spatial and temporal grouping and coordinates
load("data/spatial/mobility_regions.RData")
load("data/spatial/epsg3035.RData")

janno_spatial <- janno_phaseII_mds %>%
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
    pop = purrr::map_chr(Group_Name, function(x) { x[[1]] }),
    pup = purrr::map_chr(Publication_Status, function(x) { x[[1]] })
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

### umap

# on distances
# scaling of dimensions?
# ems

mds_umap_raw <- uwot::umap(
  mds[2:10], n_neighbors = 100, min_dist = 0.1
)
mds_umap <- tibble::tibble(D1 = mds_umap_raw[,1], D2 = mds_umap_raw[,2])

# mds_tsne_raw <- Rtsne::Rtsne(as.matrix(mds[2:10]))
# mds_umap_tsne <- mds_umap %>%
#   dplyr::mutate(
#     T1 = mds_tsne_raw$Y[,1],
#     T2 = mds_tsne_raw$Y[,2]
#   )

p <- janno_final %>%
  dplyr::bind_cols(mds_umap) %>%
  ggplot() +
  geom_point(
    aes(
      x = D1, y = D2, 
      color = pup,
      shape = region_id,
      label = pop
    ),
    size = 2
  ) +
  scale_shape_manual(
    values = region_id_shapes,
    na.value = 3
  ) +
  #age_colors_gradient +
  coord_fixed() +
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
    color = guide_none(),
    #color = guide_colorbar(title = "Time", barwidth = 20, barheight = 1.5),
    shape = guide_legend(
      title = "Region", nrow = 3, ncol = 3, byrow = T,
      override.aes = aes(size = 3, stroke = 1)
    )
  )

plotly::ggplotly(p)

### umap with distances

dist_values <- readr::read_tsv(
  "post_review_experiments/data/phaseII_package/plink.mdist",
  col_names = F,
  col_types = readr::cols(.default = readr::col_double())
) %>% as.data.frame()
dist_labels <- readr::read_tsv(
  "post_review_experiments/data/phaseII_package/plink.mdist.id",
  col_names = F,
  col_types = readr::cols(.default = readr::col_character())
)

colnames(dist_values) <- dist_labels$X2
rownames(dist_values) <- dist_labels$X2
dist_matrix <- dist_values %>% as.matrix()
#dist_matrix[lower.tri(dist_matrix)] <- NA
mydist <- as.dist(dist_matrix)

dist_umap_raw <- uwot::umap(
  mydist, n_neighbors = 100, min_dist = 0.1
)

# conf <- umap::umap.defaults
# conf$n_neighbors <- 300
# conf$min_dist <- 0.01
# dist_umap_raw_2 <- umap::umap(
#   as.matrix(mydist),
#   input = "dist",
#   config = conf
# )$layout

j <- janno_final %>% dplyr::left_join(
  tibble::as_tibble(dist_umap_raw, rownames = "Poseidon_ID"),
  by = c("Individual_ID" = "Poseidon_ID")
)

p <- ggplot() +
  geom_point(
    data = j,
    aes(
      x = V1, y = V2, 
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
