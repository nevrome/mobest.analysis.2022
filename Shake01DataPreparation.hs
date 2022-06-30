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

  want $ map dataGenotypeDataInitialSelection [ 
      "initial_selection.bed"
    ]

  -- 01

  code0101 "01_prepare_spatial_data.R" %$
    map data_tracked [
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
    map dataGenotypeDataAADRv501240K [
      "v50.0_1240k_public.anno"
    , "aadr_1240K.geno"
    , "aadr_1240K.snp"
    , "aadr_1240K.ind"
    ]

  code0101 "04_download_aadr_1240K_HO.sh" %$
    [] -->
    map dataGenotypeDataAADRv501240KHO [
      "v50.0_HO_public.anno"
    , "aadr_1240K_HO.geno"
    , "aadr_1240K_HO.snp"
    , "aadr_1240K_HO.ind"
    ]

  -- 02

  code0102 "01_janno_filter_for_relevant_individuals.R" %$
    [ dataSpatial "epsg3035.RData"
    , dataSpatial "research_area.RData"
    , dataGenotypeDataAADRv501240K "v50.0_1240k_public.anno"
    , code0102 "aadr_age_string_parser.R"
    ] -->
    [ --dataGenotypeDataAADRv501240K "aadr_poseidon.janno"
      dataGenotypeData "janno_initial_selection.RData"
    , code01 "ind_list_initial_selection.txt"
    ]

  code0102 "02_pre_identicals_filter_poseidon_extract.sh" %$
    code01 "ind_list_initial_selection.txt" : map dataGenotypeDataAADRv501240K [
    -- , "aadr_poseidon.janno"
      "aadr_1240K.geno"
    , "aadr_1240K.snp"
    , "aadr_1240K.ind"
    ] -->
    map dataGenotypeDataInitialSelection [
      "initial_selection.bed"
    , "initial_selection.bim"
    , "initial_selection.fam"
    ]

  -- 03

  code0103 "01_distance_plink.sh" %$
    code01 "myrange.txt" : map dataGenotypeDataInitialSelection [
      "initial_selection.bed"
    , "initial_selection.bim"
    , "initial_selection.fam"
    ] -->
    dataGenotypeData "janno_initial_selection.RData" :
    map dataGenotypeDataRemoveRelatedIndividuals [
      "plink.mdist"
    , "plink.mdist.id"
    ]

  code0103 "02_filter_by_genetic_distance.R" %$
    map dataGenotypeDataRemoveRelatedIndividuals [
      "plink.mdist"
    , "plink.mdist.id"
    ] -->
    [ dataGenotypeData "janno_without_identicals.RData"
    , code01 "ind_list_remove_related_selection.txt"
    ]

  code01 "05_poseidon_extract.sh" %$
    code01 "ind_list.txt" : map dataGenotypeDataAADRv501240K [
    --  "POSEIDON.yml"
      "aadr_1240K.geno"
    , "aadr_1240K.snp"
    , "aadr_1240K.ind"
    -- , "aadr_poseidon.janno"
    ] -->
    map dataGenotypeDataRemoveRelatedIndividualsSelection [
    --  "POSEIDON.yml"
      "remove_related_selection.bed"
    , "remove_related_selection.bim"
    , "remove_related_selection.fam"
    -- , "poseidon_extracted.janno"
    ]

    -- 04

    code0104 "01_purify_genotype_data.sh" %$
      code01 "myrange.txt" : map dataGenotypeDataRemoveRelatedIndividualsSelection [
      --  "POSEIDON.yml"
        "remove_related_selection.bed"
      , "remove_related_selection.bim"
      , "remove_related_selection.fam"
      -- , "poseidon_extracted.janno"
      ] -->
      map dataGenotypeDataSnpSubsets [
        "purified.bed"
      , "purified.bim"
      , "purified.fam"
      ]

    code0104 "02_edit_fam_for_assoc" %$
      [dataGenotypeDataSnpSubsets "purified.fam"] -->
      [dataGenotypeDataSnpSubsets "purified.fam"]

    code0104 "03_run_assoc.sh" %$
      map dataGenotypeDataSnpSubsets [
        "purified.bed"
      , "purified.bim"
      , "purified.fam"
      ] -->
      [dataGenotypeDataSnpSubsets "plink.assoc"]

    code0104 "04_explorse_assoc" %$
      map dataGenotypeDataSnpSubsets [
        "plink.assoc"
      , "purified.bim"
      ] -->
      [dataGenotypeDataSnpSubsets "capture_shotgun_filter.bim"]

    code0104 "05_extract_snps_identified_by_assoc" %$
       map dataGenotypeDataSnpSubsets [
        "purified.bed"
      , "purified.bim"
      , "purified.fam"
      , "capture_shotgun_filter.bim"
      ] -->
      



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