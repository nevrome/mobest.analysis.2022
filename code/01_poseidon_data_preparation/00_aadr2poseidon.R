library(magrittr)

aadr_raw <- readr::read_tsv("https://reichdata.hms.harvard.edu/pub/datasets/amh_repo/curated_releases/V50/V50.0/SHARE/public.dir/v50.0_1240K_public.anno", na = c("", ".."))

aadr_age_string <- aadr_raw$`Full Date: One of two formats. (Format 1) 95.4% CI calibrated radiocarbon age (Conventional Radiocarbon Age BP, Lab number) e.g. 2624-2350 calBCE (3990±40 BP, Ua-35016). (Format 2) Archaeological context range, e.g. 2500-1700 BCE`

aadr_minimal_janno <- aadr_raw %>%
  dplyr::select(
    Individual_ID = `Version ID`,
    Latitude = Lat.,
    Longitude = Long.,
    Date_BP_Median_Derived = `Date mean in BP in years before 1950 CE [OxCal mu for a direct radiocarbon date, and average of range for a contextual date]`,
    Nr_autosomal_SNPs = `SNPs hit on autosomal targets`,
    Xcontam = `Xcontam ANGSD MOM point estimate (only if male and ≥200)`,
    Genetic_Sex = Sex,
    ASSESSMENT
  ) %>%
  dplyr::mutate(
    Date_BC_AD_Median_Derived = -Date_BP_Median_Derived + 1950
  )

split_age_string <- function(x) {
  
  #### modify input (fixing small details) ####
  x <- gsub("\\+", "\u00B1", x) # replace + with \u00B1
  #x <- gsub("(.*)(?=\\()", "\\1 ", x, perl = T) # missing space before parentheses
  x <- gsub("AA-R-", "AAR- ", x) # fix wrong lab code (http://www.radiocarbon.org/Info/labcodes.html)
  x <- gsub("Wk - ", "Wk-", x) # fix wrong lab code
  x <- gsub("\u{00a0}", " ", x) # replace wrong space characters
  x <- gsub("¬†", " ", x)
  
  #### construct result table ####
  res <- tibble::tibble(
    x = x,
    Date_C14_Labnr = rep(NA, length(x)),
    Date_C14_Uncal_BP = NA,
    Date_C14_Uncal_BP_Err = NA,
    Date_BC_AD_Start = NA,
    Date_BC_AD_Stop = NA,
    Date_Type = NA
  )
  
  #### first rough determination of dating type info ####
  none_ids <- which(is.na(x))
  res$Date_Type[none_ids] <- "none"
  present_ids <- grep("present", x)
  res$Date_Type[present_ids] <- "modern"
  c14_age_ids <- grep("±", x) # indizes of suspected radiocarbon dates

  #### parse uncalibrated c14 dates ####
  # extract real, nice radiocarbon dates
  full_radiocarbon_dates <- stringr::str_extract_all(
    x[c14_age_ids], 
    paste0(
      "[0-9]{1,5}(\\s+)*\u00B1(\\s+)*[0-9]{1,4}( BP)*,\\s{0,1}(", # pattern for age +/- std
      paste(
        c( # patterns for labnrs
          "CNA4579.1.1",
          "Beta [0-9]+",
          "COL3897.1.1",
          "A-",
          "D-AMS-[0-9]*",
          "AA84155",
          "KIA44691",
          "MAMS 21972",
          "R-EVA1606/MAMS-[0-9]*",
          "TÜBİTAK-[0-9]*",
          "AAR-\\s[0-9]*",
          "OxA-X-[0-9]*-[0-9]*",
          "CIRCE-DSH-[0-9]*",
          "ISGS-A[0-9]*",
          "CEDAD-LTL[0-9]*A",
          "[A-Za-z]{2,7}-*[0-9]*" # that's the normal pattern, the others are deviating from that
        ),
        collapse = "|"
      ),
      ")"
    )
  )
  
  # if there is no real, nice date, then it can't be a proper C14 date at all
  full_radiocarbon_date_consumed <- purrr::map_lgl(full_radiocarbon_dates, function(x) {length(x) > 0})
  c14_age_ids_true <- c14_age_ids[full_radiocarbon_date_consumed]

  # split date and labnr
  radiocarbon_split <- purrr::map(full_radiocarbon_dates[full_radiocarbon_date_consumed], function(y) {
    stringr::str_split(y, c("\u00B1|( BP){0,1}, ")) %>% 
      purrr::transpose(c("uncal_age", "uncal_std", "labnr")) %>%
      purrr::map(unlist)
  }) %>% purrr::transpose()

  # write uncalibrated dates into the result table
  res$Date_C14_Uncal_BP[c14_age_ids_true] <- radiocarbon_split$uncal_age
  res$Date_C14_Uncal_BP_Err[c14_age_ids_true] <- radiocarbon_split$uncal_std
  res$Date_C14_Labnr[c14_age_ids_true] <- radiocarbon_split$labnr
  
  #
  c14_age_ids_false <- c14_age_ids[!full_radiocarbon_date_consumed]
  x[c14_age_ids_false]
  
  
  # indizes of real, nice radiocarbon dates
  res$Date_Type[c14_age_ids_true] <- "C14"
  res$Date_Type[is.na(res$Date_Type)] <- "contextual"
  
  
  #### parse contextual (and simplified) ages ####
  
  # split at space and minus
  simple_age_split <- x %>% stringr::str_split("\\s-\\s|-|\\s+")
  
  # translate first elements of the vector to meaningful start and stop ages
  stop <- start <- rep(NA, length(simple_age_split))
  for (i in 1:length(simple_age_split)) {
    # no age info
    if (is.na(simple_age_split[[i]][1])) {
      start[i] <- NA
      stop[i] <- NA
      next
    }
    # age beyond calibration range, e.g. >45000
    if (grepl("^>", simple_age_split[[i]][1])) {
      start[i] <- -Inf
      stop[i] <- as.numeric(gsub(">", "", simple_age_split[[i]][1]))
      next
    }
    # no range: only one value e.g. 5000 BCE
    if (length(simple_age_split[[i]]) == 2) {
      if (simple_age_split[[i]][2] == "BCE") {
        start[i] <- -as.numeric(simple_age_split[[i]][1])
        stop[i] <- -as.numeric(simple_age_split[[i]][1])
        next
      }
      if (simple_age_split[[i]][2] == "CE") {
        start[i] <- as.numeric(simple_age_split[[i]][1])
        stop[i] <- as.numeric(simple_age_split[[i]][1])
        next
      } 
      if (all(grepl("^[0-9]+$", simple_age_split[[i]]))) {
        start[i] <- -as.numeric(simple_age_split[[i]][1])
        stop[i] <- -as.numeric(simple_age_split[[i]][2])
        next
      }
    }
    # normal range 5000-4700 BCE
    if (simple_age_split[[i]][3] %in% c("BCE", "calBCE")) {
      start[i] <- -as.numeric(simple_age_split[[i]][1])
      stop[i] <- -as.numeric(simple_age_split[[i]][2])
      next
    }
    if (simple_age_split[[i]][3] %in% c("CE", "calCE")) {
      start[i] <- as.numeric(simple_age_split[[i]][1])
      stop[i] <- as.numeric(simple_age_split[[i]][2])
      next
    }
    if (simple_age_split[[i]][2] %in% c("BCE", "calBCE") & simple_age_split[[i]][4] %in% c("CE", "calCE")) {
      start[i] <- -as.numeric(simple_age_split[[i]][1])
      stop[i] <- as.numeric(simple_age_split[[i]][3])
      next
    }
  }
  
  # write start and to the columns
  res$Date_BC_AD_Start <- start
  res$Date_BC_AD_Stop <- stop
  
  return(res)
}

split_age_string(aadr_age_string) %>% View()
