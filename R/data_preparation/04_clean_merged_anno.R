#### load dataset ####
load("data/anno_1240K_and_anno_1240K_HumanOrigins_raw.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_raw

#### clean and restructure temporal information

source("R/data_preparation/helper_functions.R")

anno <- anno %>%
  # add age cols
  dplyr::bind_cols(
    reconstruct_age_info(anno$age_string)
  )

anno$age_prob_distribution_BC <- lapply(anno$sample_id, function(x) tibble::tibble(age = numeric(), prob = numeric()))

# archaeological dating: uniform distribution
anno[!is.na(anno$age_arch_start_BC) & !is.na(anno$age_arch_stop_BC),] %<>% dplyr::mutate(
  age_prob_distribution_BC = purrr::map2(
    age_arch_start_BC, age_arch_stop_BC, function(start, stop) {
      time_window <- start:stop
      prop_in_window <- 1/length(time_window)
      tibble::tibble(
        age = time_window,
        prob = prop_in_window
      )
    }
  )
)

# c14 dating: cal-distribution
anno[!is.na(anno$age_c14_uncal_BP) & !is.na(anno$age_c14_uncal_BP_dev),] %<>% dplyr::mutate(
  age_prob_distribution_BC =
    Bchron::BchronCalibrate(
      ages      = age_c14_uncal_BP,
      ageSds    = age_c14_uncal_BP_dev,
      calCurves = rep("intcal13", nrow(.))
    ) %>%
    purrr::map(
      function(x) {
        dplyr::arrange(
          tibble::tibble(
            age = -x[["ageGrid"]] + 1950, 
            prob = x[["densities"]]
          ), 
          age
        )
      }
    )
)

# reduce age ranges to the 95% highest density region: https://stats.stackexchange.com/questions/240749/how-to-find-95-credible-interval
anno$age_prob_distribution_BC_95 <- anno$age_prob_distribution_BC %>%
  lapply(
    function(x) {
      if (nrow(x) < 1) {
        return(tibble::tibble(age = as.numeric(), prob = as.numeric()))
      } else {
        samp <- sample(x$prob, 1e5, replace = TRUE, prob = x$prob)
        crit <- quantile(samp, 0.05)
        x[x$prob >= crit, ]
      }
    }
  )

# get oldest and youngest age of reduced range
anno %<>%
  dplyr::mutate(
    age_start_BC = purrr::map_dbl(age_prob_distribution_BC_95, function(x) {
      if (nrow(x) < 1) { return(NA) } else { x$age[1] }
    }),
    age_stop_BC = purrr::map_dbl(age_prob_distribution_BC_95, function(x) {
      if (nrow(x) < 1) { return(NA) } else { x$age[nrow(x)] }
    })
  )

#### finish and store modified anno file ####
anno_1240K_and_anno_1240K_HumanOrigins <- anno
save(anno_1240K_and_anno_1240K_HumanOrigins, file = "data/anno_1240K_and_anno_1240K_HumanOrigins.RData")
