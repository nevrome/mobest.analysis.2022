library(magrittr)

# remove incorrectly encoded characters
# iconv -f ISO8859-1 -t UTF8 1240K_HumanOrigins.anno > 1240K_HumanOrigins.anno.mod
# sed 's/[Ê]/\ /g' 1240K_HumanOrigins.anno.mod > 1240K_HumanOrigins.anno.mod2

anno_1240K_HumanOrigins_raw <- readr::read_tsv("data/1240K_HumanOrigins/1240K_HumanOrigins.anno.mod2", na = c("-", "..", "n/a"))

#### simplifiy rownames and clean/restructure some rows ####
anno_1240K_HumanOrigins <- anno_1240K_HumanOrigins_raw %>%
  dplyr::transmute(
    index = Index,
    sample_id = `Sample_ID`,
    master_id = `Master_ID`,
    sex = Sex,
    group_label = `Group Label`,
    group_label_verbose = `Verbose Group Label (Modern) or Skeletal Type (Ancient)`,
    data_source = `Data source`,
    reference = `Reference for first report of data or OK to use in a paper`,
    contributor = `Contributor or published source`,
    location = Location,
    site = Site,
    country = Country,
    lat = Lat.,
    lon = Long.,
    coverage = Coverage,
    snps_hit_on_autosomes = `SNPs hit on autosomes`,
    udg = `UDG`,
    restrictions = `Restrictions (0=none; 1=Basic signed letter; 2=Restrictive signed letter; 3=Bolnick signed letter; 4=Never release; 5=Disease patient)`,
    assessment = `Assessment`
  )

#### store dataset ####
save(anno_1240K_HumanOrigins, file = "data/anno_1240K_HumanOrigins.RData")
