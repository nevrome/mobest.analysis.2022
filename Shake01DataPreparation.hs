#!/usr/bin/env stack
{- stack script
 --resolver lts-18.28 
 --package shake,filepath
 -}

import ShakeUtils
import Development.Shake

main :: IO ()
main = shakeArgs myShakeOpts $ do

  -- want $ map figures [ 
  --     -- "research_area.RData"
  --     "figure_1_temporal_and_spatial_distribution_of_input_data.pdf"
  --   ]

  want $ [dataGenoSnpSubUnfilteredModern "unfiltered_snp_selection_with_modern_reference_pops.bed"]

  -- 01

  code0101 "01_prepare_spatial_data.R" %$
    map dataTracked [
      "research_area/research_area.gpkg"
    , "natural_earth_geodata/land_outline.RData"
    , "natural_earth_geodata/rivers.RData"
    , "natural_earth_geodata/lakes.RData"
    , "mobility_regions/mobility_regions.gpkg"
    ] -->
    map dataSpatial [
      "research_area.RData"
    , "epsg3035.RData"
    , "land_outline.RData"
    , "rivers.RData"
    , "lakes.RData"
    , "area.RData"
    , "extended_area.RData"
    , "mobility_regions.RData"
    ]

  code0101 "02_prepare_plot_reference_data.R" %$
    [] -->
    map dataPlotReferenceData [
      "region_id_shapes.RData"
    , "age_colors_gradient.RData"
    ]

  code0101 "03_download_aadr_1240K.sh" %$
    [] -->
    map dataGenoAADRv501240K [
      "v50.0_1240k_public.anno"
    , "aadr_1240K.geno"
    ]

  code0101 "04_download_aadr_1240K_HO.sh" %$
    [] -->
    map dataGenoAADRv501240KHO [
      "aadr_1240K_HO.geno"
    ]

  -- 02

  code0102 "01_janno_filter_for_relevant_individuals.R" %$
    [ dataSpatial "epsg3035.RData"
    , dataSpatial "research_area.RData"
    , dataGenoAADRv501240K "v50.0_1240k_public.anno"
    , code0102 "aadr_age_string_parser.R"
    ] -->
    [ dataGeno "janno_initial_selection.RData"
    , code01 "ind_list_initial_selection.txt"
    ]

  code0102 "02_pre_identicals_filter_poseidon_extract.sh" %$
    [ code01 "ind_list_initial_selection.txt"
    , dataGenoAADRv501240K "aadr_1240K.geno"
    ] -->
    [ dataGenoInitialSelection "initial_selection.bed" ]

  -- 03

  code0103 "01_distance_plink.sh" %$
    [ code01 "myrange.txt"
    , dataGenoInitialSelection "initial_selection.bed"
    ] -->
    dataGeno "janno_initial_selection.RData" :
    map dataGenoRemoveRelatedIndividuals [
      "plink.mdist"
    , "plink.mdist.id"
    ]

  code0103 "02_filter_by_genetic_distance.R" %$
    map dataGenoRemoveRelatedIndividuals [
      "plink.mdist"
    , "plink.mdist.id"
    ] -->
    [ dataGeno "janno_without_identicals.RData"
    , code01 "ind_list_remove_related_selection.txt"
    ]

  code01 "05_poseidon_extract.sh" %$
    [ code01 "ind_list.txt"
    , dataGenoAADRv501240K "aadr_1240K.geno"
    ] -->
    [ dataGenoRemoveRelatedIndividualsSelection "remove_related_selection.bed" ]

  -- 04

  code0104 "01_purify_genotype_data.sh" %$
    [ code01 "myrange.txt"
    , dataGenoRemoveRelatedIndividualsSelection "remove_related_selection.bed"
    ] -->
    [ dataGenoSnpSub "purified.bed" ]

  code0104 "02_edit_fam_for_assoc" %$
    [ dataGenoSnpSub "purified.fam" ] -->
    [ dataGenoSnpSub "purified.fam" ]

  code0104 "03_run_assoc.sh" %$
    [ dataGenoSnpSub "purified.bed" ] -->
    [ dataGenoSnpSub "plink.assoc" ]

  code0104 "04_explorse_assoc" %$
    map dataGenoSnpSub [
      "plink.assoc"
    , "purified.bim"
    ] -->
    [ dataGenoSnpSub "capture_shotgun_filter.bim" ]

  code0104 "05_extract_snps_identified_by_assoc.sh" %$
     map dataGenoSnpSub [
      "purified.bed"
    , "capture_shotgun_filter.bim"
    ] -->
    [ dataGenoSnpSubUnfilteredPre "purified.bed"
    , dataGenoSnpSubFilteredPre "filtered_snp_selection_pre_ind_correction.bed"
    ]

  code0104 "06_find_individuals_with_sufficient_snp_count.R" %$
    [ dataGenoSnpSubFilteredPre "filtered_snp_selection_pre_ind_correction.janno" ]
    --> 
    [ dataGeno "post_snp_selection_individuals.RData"
    , code01 "ind_list_post_snp_selection.txt"
    ]

  code0104 "07_extract_inviduals_with_enough_snps.sh" %$
    [ dataGenoSnpSubUnfilteredPre "purified.bed"
    , dataGenoSnpSubFilteredPre "filtered_snp_selection_pre_ind_correction.bed"
    ] -->
    [ dataGenoSnpSubUnfiltered "unfiltered_snp_selection.bed"
    , dataGenoSnpSubFiltered "filtered_snp_selection.bed"
    ]

  code0104 "08_create_merge_with_modern_reference_pops.sh" %$
    [ dataGenoAADRv501240KHO "aadr_1240K_HO.geno"
    , dataGenoSnpSubUnfiltered "unfiltered_snp_selection.bed"
    , dataGenoSnpSubFiltered "filtered_snp_selection.bed"
    ] -->
    [ dataGenoAADRModern "aadrv50_1240K_HO_Western_Eurasia_modern"
    , dataGenoSnpSubUnfilteredModern "unfiltered_snp_selection_with_modern_reference_pops.bed"
    , dataGenoSnpSubFilteredModern "filtered_snp_selection_with_modern_reference_pops.bed"
    ]

  --05







  -- code01 "06_mds_plink.shlm" %$
  --   code01 "myrange.txt" : map dataPoseidonDataPoseidonExtracted [
  --     "poseidon_extracted.bed"
  --   , "poseidon_extracted.bim"
  --   , "poseidon_extracted.fam"
  --   ] -->
  --   [ dataPoseidonDataMDS "mds.mds" ]

  -- code01 "07_prepare_final_dataset.R" %$
  --   [ dataSpatial "epsg3035.RData"
  --   , dataSpatial "mobility_regions.RData"
  --   , dataPoseidonDataPoseidonExtracted "poseidon_extracted.janno"
  --   , dataPoseidonDataMDS "mds.mds"
  --   ] -->
  --   [ dataPoseidonData "janno_final.RData" ]

  -- -- plots
  -- code04Paper "figure_1_temporal_and_spatial_distribution_of_input_data.R" %$
  --   [ dataPoseidonData "janno_final.RData"
  --   , dataSpatial "research_area.RData"
  --   , dataSpatial "extended_area.RData"
  --   , dataSpatial "epsg3035.RData"
  --   , dataSpatial "mobility_regions.RData"
  --   , dataPlotReferenceData "region_id_shapes.RData"
  --   , dataPlotReferenceData "age_colors_gradient.RData"
  --   ] -->
  --   [ plots "figure_1_temporal_and_spatial_distribution_of_input_data.pdf" ]