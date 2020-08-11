library(magrittr)

#### load dataset ####
load("data/anno_1240K_and_anno_1240K_HumanOrigins_raw.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_raw

#### clean and restructure temporal information ####
source("code/data_preparation/helper_functions.R")

# clean age string
anno <- anno %>%
  # add age cols
  dplyr::bind_cols(
    reconstruct_age_info(anno$age_string)
  )

# anno <- tibble::tibble(
#   age_c14_uncal_BP = c(1000, 3000, NA, 5000),
#   age_c14_uncal_BP_dev = c(20, 40, NA, 50),
#   age_arch_start_BC = c(NA, NA, -3000, NA),
#   age_arch_stop_BC = c(NA, NA, -2500, NA),
#   age_prob_distribution_BC = lapply(
#     age_c14_uncal_BP, function(x) {
#       tibble::tibble(age = numeric(), dens_dist = numeric(), norm_dens = numeric(), center = numeric())
#     }
#   )
# )

# determine dating type
anno$age_type <- NA
anno$age_type[!is.na(anno$age_arch_start_BC) & !is.na(anno$age_arch_stop_BC)] <- "context"
anno$age_type[!is.na(anno$age_c14_uncal_BP) & !is.na(anno$age_c14_uncal_BP_dev)] <- "C14"

# age probability distribution list column template
anno$age_prob_distribution_BC <- lapply(anno$sample_id, function(x) tibble::tibble(age = numeric(), dens_dist = numeric(), norm_dens = numeric(), center = numeric()))

# archaeological dating: uniform distribution
anno[!is.na(anno$age_arch_start_BC) & !is.na(anno$age_arch_stop_BC),] %<>% dplyr::mutate(
  age_prob_distribution_BC = purrr::map2(
    age_arch_start_BC, age_arch_stop_BC, function(start, stop) {
      time_window <- start:stop
      prop_in_window <- 1/length(time_window)
      tibble::tibble(
        age = time_window,
        dens_dist = prop_in_window,
        norm_dens = prop_in_window/max(prop_in_window),
        center = time_window == time_window[which.min(abs(time_window - mean(time_window)))]
      )
    }
  )
)

# c14 dating: calibrated distribution
bol <- 1950 # c14 reference zero
threshold <- (1 - 0.9545) / 2 # 2sigma range probability threshold
anno[!is.na(anno$age_c14_uncal_BP) & !is.na(anno$age_c14_uncal_BP_dev),] %<>% dplyr::mutate(
  age_prob_distribution_BC =
    Bchron::BchronCalibrate(
      ages      = age_c14_uncal_BP,
      ageSds    = age_c14_uncal_BP_dev,
      calCurves = rep("intcal13", nrow(.))
    ) %>%
    # transform BchronCalibrate result to a informative tibble
    # this tibble includes the years, the density per year,
    # the normalized density per year and the information,
    # if this year is in the two_sigma range for the current date
    pbapply::pblapply(
      function(x) {
        a <- x$densities %>% cumsum # cumulated density
        bottom <- x$ageGrid[which(a <= threshold) %>% max]
        top <- x$ageGrid[which(a > 1 - threshold) %>% min]
        center <- x$ageGrid[which.min(abs(a - 0.5))]
        tibble::tibble(
          age = -x$ageGrid + bol,
          dens_dist = x$densities,
          norm_dens = x$densities/max(x$densities),
          two_sigma = x$ageGrid >= bottom & x$ageGrid <= top,
          center = x$ageGrid == center
        )
      }
    )
)

# add center age column
anno$calage_center <- sapply(anno$age_prob_distribution_BC, function(x) {
  if (nrow(x) == 0) {
    NA
  } else {
    x$age[x$center]
  }
})

# temporal sampling
anno$calage_sample <- lapply(
  anno$age_prob_distribution_BC, function(x) {
    if (nrow(x) < 2) {
      NA
    } else {
      sample(x$age, 1000, replace = TRUE, prob = x$dens_dist)
    }
  }
)

#### finish and store modified anno file ####
anno_1240K_and_anno_1240K_HumanOrigins <- anno
save(anno_1240K_and_anno_1240K_HumanOrigins, file = "data/anno_1240K_and_anno_1240K_HumanOrigins.RData")
