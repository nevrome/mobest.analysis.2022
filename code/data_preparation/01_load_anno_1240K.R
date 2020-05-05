library(magrittr)

# remove incorrectly encoded characters
# iconv -f ISO8859-1 -t UTF8 1240K.anno > 1240K.anno.mod
# sed 's/[Ê]/\ /g' 1240K.anno.mod > 1240K.anno.mod2

anno_1240K_raw <- readr::read_tsv("data/1240K/1240K.anno.mod2", na = c("..", "n/a"))

#### simplifiy rownames and clean/restructure some rows ####
anno_1240K <- anno_1240K_raw %>%
  dplyr::transmute(
    index = Index,
    instance_id = `Instance ID`,
    master_id = `Master ID`,
    skeletal_code = `Skeletal code`,
    skeletal_element = `Skeletal element`,
    library_ids = `LibraryID(s)` %>% strsplit(split = ","),
    library_number = `No. Libraries`,
    data_type = `Data type`,
    age_mean_calBP = `Average of 95.4% date range in calBP (defined as 1950 CE)`,
    group_label = `Group label`,
    location = Location,
    site = Site,
    country = Country,
    lat = Lat.,
    lon = Long.,
    sex = Sex,
    mt_dna_best = `mtDNA best (>2x)`,
    y_chrom_calls = `Y chrom. calls`,
    y_chrom_curation = `Y chrom. curation`,
    percent_endogenous_shotgun_best_library = `% endogenous in shotgun sequencing for the best library`,
    coverage = Coverage,
    snps_hit_on_autosomes = `SNPs hit on autosomes`,
    udg_treatment = `UDG treatment (minus=untreated; half=treated except in last nucleotides; plus=treated over all nucleotides)` %>% strsplit(","),
    damage_restrict = `Damage restrict?`,
    assessment_decision = ifelse(
      grepl("PASS", `ASSESSMENT (Xcontam: flag anything with |Z|>2: 0.02-0.05 QUESTIONABLE and >0.05 "FAIL" and not quoted if <200 SNPs or not "M"; mtcontam: flag QUESTIONABLE if <0.95 and not overridden by Xcontam, mark if <0.98, flag "QUESTIONABLE_CRITICAL" if mtcontam on component libraries is <0)`),
      "PASS",
      ifelse(
        grepl("QUESTIONABLE", `ASSESSMENT (Xcontam: flag anything with |Z|>2: 0.02-0.05 QUESTIONABLE and >0.05 "FAIL" and not quoted if <200 SNPs or not "M"; mtcontam: flag QUESTIONABLE if <0.95 and not overridden by Xcontam, mark if <0.98, flag "QUESTIONABLE_CRITICAL" if mtcontam on component libraries is <0)`),
        "QUESTIONABLE",
        "CRITICAL"
      )
    ),
    assessment_comment = 
      `ASSESSMENT (Xcontam: flag anything with |Z|>2: 0.02-0.05 QUESTIONABLE and >0.05 "FAIL" and not quoted if <200 SNPs or not "M"; mtcontam: flag QUESTIONABLE if <0.95 and not overridden by Xcontam, mark if <0.98, flag "QUESTIONABLE_CRITICAL" if mtcontam on component libraries is <0)` %>%
        purrr::map2_chr(., assessment_decision, function(x, y) {gsub(y, "", x)}) %>% trimws(),
    library_endogenous = `endogenous by library` %>% strsplit(",") %>% lapply(function(x) {as.numeric(x)}),
    library_damage = `damage by library` %>% strsplit(",") %>% lapply(function(x) {as.numeric(x)}),
    library_mt_haplogroup = `mt.haplogroup by library (update to Shop's new mt-caller)` %>% strsplit(","),
    library_match_rate_to_consensus = `mt.match rate to consensus by library` %>% strsplit(",") %>% lapply(function(x) {as.numeric(x)}),
    age_string = `Date: One of two formats. (Format 1) 95.4% CI calibrated radiocarbon age (Conventional Radiocarbon Age B, Lab number) e.g. 5983-5747 calBCE (6980±50 B B, Beta-226472). (Format 2) Archaeological context date B, e.g. 2500-1700 BCE`
  )

#### store dataset ####
save(anno_1240K, file = "data/anno_1240K.RData")

