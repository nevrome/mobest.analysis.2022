library(magrittr)

load("data/genotype_data/janno_final.RData")
load("data/origin_search/packed_origin_vectors.RData")

# sample context table
table_sample_context <- janno_final %>%
  dplyr::mutate(
    Group_Name = purrr::map_chr(
      Group_Name,
      \(x) x[[1]]
    ),
    Publication = purrr::map_chr(
      Publication, 
      \(x) x[[1]] %>% gsub("\\(.*$", "", .) %>% trimws
    ),
    dplyr::across(
      c(Latitude, Longitude),
      \(x) round(x, 3)
    ),
    Date_C14 = purrr::pmap_chr(
      list(Date_C14_Labnr, Date_C14_Uncal_BP, Date_C14_Uncal_BP_Err),
      function(labnrs, bps, stds) {
        if (all(is.na(labnrs))) {
          return(NA_character_)
        } else {
          purrr::pmap_chr(
            list(labnrs, bps, stds),
            function(labnr, bp, std) {
              paste0("(", labnr, ":", bp, "±", std, ")")
            }
          ) %>% paste(collapse = ";")
        }
      }
    )
  ) %>%
  dplyr::select(
    Sample_ID = Poseidon_ID,
    Genetic_Sex,
    Group_Name,
    Publication,
    Country,
    Region = region_id,
    Latitude,
    Longitude,
    Date_BC_AD_Start = Date_BC_AD_Start_Derived,
    Date_BC_AD_Median = Date_BC_AD_Median_Derived,
    Date_BC_AD_Stop = Date_BC_AD_Stop_Derived,
    Date_C14,
    Age_Group = age_group_id
  )

# multivar result table

# check ranges per column
# janno_final %>%
#   dplyr::select(
#     Sample_ID = Poseidon_ID,
#     tidyselect::matches("^C[0-9]+_.*")
#   ) %>%
#   dplyr::group_by() %>%
#   dplyr::summarise(
#       dplyr::across(
#         tidyselect:::where(is.numeric),
#         range
#         )
#       ) %>%
#   t() %>%
#   View()

table_multivar_results <- janno_final %>%
  dplyr::select(
    Sample_ID = Poseidon_ID,
    tidyselect::matches("^C[0-9]+_.*")
  ) %>%
  dplyr::mutate(
    dplyr::across(
      tidyselect::contains(c("mds", "emu")),
      \(x) round(x, 5)
    ),
    dplyr::across(
      tidyselect::contains("pca"),
      \(x) round(x, 3)
    )
  )

# origin search result table
table_origin_search_results_unordered <- packed_origin_vectors %>%
  dplyr::rename(Sample_ID = search_id) %>%
  dplyr::mutate(
    search_time = dplyr::case_when(
      search_time == max(search_time) ~ "retro_high",
      search_time == min(search_time) ~ "retro_low",
      TRUE ~ "retro_default"
    )
  ) %>%
  tidyr::pivot_wider(
    id_cols = tidyselect::all_of(c("Sample_ID", "search_x", "search_y", "search_z")),
    names_from = tidyselect::all_of(c("multivar_method", "search_time")),
    values_from = tidyselect::matches(c("^field_.*", "^ov_.*"))
  ) %>%
  dplyr::mutate(
    dplyr::across(
      tidyselect:::where(is.numeric), 
      as.integer
    )
  )

table_origin_search_results <- table_origin_search_results_unordered[
  order(
    match(
      table_origin_search_results_unordered$Sample_ID,
      table_sample_context$Sample_ID
    )
  ),
]

# write results
table_sample_context %>%
  readr::write_csv(
    "tables/table_sup_1_sample_context.csv",
    na = ""
  )

table_multivar_results %>%
  readr::write_csv(
    "tables/table_sup_2_multivar_results.csv",
    na = ""
  )

table_origin_search_results %>%
  readr::write_csv(
    "tables/table_sup_3_search_results.csv",
    na = ""
  )

