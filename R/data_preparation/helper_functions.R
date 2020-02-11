reconstruct_age_info <- function(x) {
  
  res <- tibble::tibble(
    age_arch_start_BC = rep(NA, length(x)),
    age_arch_stop_BC = NA,
    age_c14_uncal_BP = NA,
    age_c14_uncal_BP_dev = NA,
    age_c14_labnr = NA
  )
  
  # c14 age
  c14_age_ids <- grep("\\(", x)
  res$age_c14_labnr[c14_age_ids] <- stringr::str_extract(x[c14_age_ids], "[A-Z,a-z]{2,7}-[0-9]*")
  uncal_date <- stringr::str_extract(x[c14_age_ids], "[0-9]{1,5}±[0-9]{1,4}") %>% strsplit("±")
  res$age_c14_uncal_BP[c14_age_ids] <- as.numeric(sapply(uncal_date, function(x) x[1]))
  res$age_c14_uncal_BP_dev[c14_age_ids] <- as.numeric(sapply(uncal_date, function(x) x[2]))
  
  # arch age
  arch_age_split <- x[-c14_age_ids] %>% strsplit("-|\\ ") %>% lapply(function(y) {y[y != ""]})
  stop <- start <- rep(NA, length(arch_age_split))
  for (i in 1:length(arch_age_split)) {
    if (is.na(arch_age_split[[i]][1])) {
      start[i] <- NA
      stop[i] <- NA
      next
    }
    if (arch_age_split[[i]][1] == "present") {
      start[i] <- 2000
      stop[i] <- 2000
      next
    }
    if (length(arch_age_split[[i]]) == 2) {
      if (arch_age_split[[i]][2] == "BCE") {
        start[i] <- -as.numeric(arch_age_split[[i]][1])
        stop[i] <- -as.numeric(arch_age_split[[i]][1])
        next
      }
      if (arch_age_split[[i]][2] == "CE") {
        start[i] <- as.numeric(arch_age_split[[i]][1])
        stop[i] <- as.numeric(arch_age_split[[i]][1])
        next
      } 
      if (all(grepl("^[0-9]+$", arch_age_split[[i]]))) {
        start[i] <- -as.numeric(arch_age_split[[i]][1])
        stop[i] <- -as.numeric(arch_age_split[[i]][2])
        next
      }
    }
    if (arch_age_split[[i]][3] == "BCE") {
      start[i] <- -as.numeric(arch_age_split[[i]][1])
      stop[i] <- -as.numeric(arch_age_split[[i]][2])
      next
    }
    if (arch_age_split[[i]][3] == "CE") {
      start[i] <- as.numeric(arch_age_split[[i]][1])
      stop[i] <- as.numeric(arch_age_split[[i]][2])
      next
    }
    if (arch_age_split[[i]][2] == "BCE" & arch_age_split[[i]][4] == "CE") {
      start[i] <- -as.numeric(arch_age_split[[i]][1])
      stop[i] <- as.numeric(arch_age_split[[i]][3])
      next
    }
  }
  res$age_arch_start_BC[-c14_age_ids] <- start
  res$age_arch_stop_BC[-c14_age_ids] <- stop
  
  return(res)
}

