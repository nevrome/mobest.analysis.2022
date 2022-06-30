#!/usr/bin/env stack
{- stack script
 --resolver lts-18.28 
 --package shake,filepath
 -}

import ShakeUtils

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.FilePath (takeExtension)

-- #### pipeline #### --

main :: IO ()
main = shakeArgs myShakeOpts $ do

  -- want $ map figures [ 
  --     -- "research_area.RData"
  --     "figure_1_temporal_and_spatial_distribution_of_input_data.pdf"
  --   ]

  want $ map dataSpatial [ 
      "research_area.RData"
    ]

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

  code01 "00_prepare_plot_reference_data.R" %$
    [] -->
    map dataPlotReferenceData [
      "region_id_shapes.RData"
    , "age_colors_gradient.RData"
    ]

  code01 "00_download_aadr.sh" %$
    [] -->
    map dataPoseidonDataAADRv50 [
      "v50.0_1240k_public.anno"
    , "aadr_eig.geno"
    , "aadr_eig.snp"
    , "aadr_eig.ind"
    ]

  code01 "01_janno_filter_for_relevant_individuals.R" %$
    [ dataSpatial "epsg3035.RData"
    , dataSpatial "research_area.RData"
    , dataPoseidonDataAADRv50 "v50.0_1240k_public.anno"
    , dataPoseidonDataAADRv50 "aadr_eig.geno"
    , dataPoseidonDataAADRv50 "aadr_eig.geno"
    , dataPoseidonDataAADRv50 "aadr_eig.geno"
    , code01 "00_aadr_age_string_parser.R"
    ] -->
    [ code01 "pre_identicals_filter_ind_list.txt"
    , dataPoseidonData "janno_pre_mds.RData"
    , dataPoseidonDataAADRv50 "aadr_poseidon.janno"
    , dataPoseidonDataAADRv50 "POSEIDON.yml"
    ]

  code01 "02_pre_identicals_filter_poseidon_extract.sh" %$
    code01 "pre_identicals_filter_ind_list.txt" : map dataPoseidonDataAADRv50 [
      "POSEIDON.yml"
    , "aadr_poseidon.janno"
    , "aadr_eig.geno"
    , "aadr_eig.snp"
    , "aadr_eig.ind"
    ] -->
    map dataPoseidonDataPoseidonExtractedPreIdenticalsFilter [
      "poseidon_extracted_pre_identicals_filter.bed"
    , "poseidon_extracted_pre_identicals_filter.bim"
    , "poseidon_extracted_pre_identicals_filter.fam"
    ]

  code01 "03_distance_plink.sh" %$
    code01 "myrange.txt" : map dataPoseidonDataPoseidonExtractedPreIdenticalsFilter [
      "poseidon_extracted_pre_identicals_filter.bed"
    , "poseidon_extracted_pre_identicals_filter.bim"
    , "poseidon_extracted_pre_identicals_filter.fam"
    ] -->
    map dataPoseidonDataIdenticalFilter [
      "plink.mdist"
    , "plink.mdist.id"
    ]

  code01 "04_filter_by_genetic_distance.R" %$
    [ dataPoseidonData "janno_pre_mds.RData"
    , dataPoseidonDataIdenticalFilter "plink.mdist"
    , dataPoseidonDataIdenticalFilter "plink.mdist.id"
    ] -->
    [ dataPoseidonData "janno_without_identicals.RData"
    , code01 "ind_list.txt"
    ]

  code01 "05_poseidon_extract.sh" %$
    code01 "ind_list.txt" : map dataPoseidonDataAADRv50 [
      "POSEIDON.yml"
    , "aadr_eig.geno"
    , "aadr_eig.snp"
    , "aadr_eig.ind"
    , "aadr_poseidon.janno"
    ] -->
    map dataPoseidonDataPoseidonExtracted [
      "POSEIDON.yml"
    , "poseidon_extracted.bed"
    , "poseidon_extracted.bim"
    , "poseidon_extracted.fam"
    , "poseidon_extracted.janno"
    ]

  code01 "06_mds_plink.shlm" %$
    code01 "myrange.txt" : map dataPoseidonDataPoseidonExtracted [
      "poseidon_extracted.bed"
    , "poseidon_extracted.bim"
    , "poseidon_extracted.fam"
    ] -->
    [ dataPoseidonDataMDS "mds.mds" ]

  code01 "07_prepare_final_dataset.R" %$
    [ dataSpatial "epsg3035.RData"
    , dataSpatial "mobility_regions.RData"
    , dataPoseidonDataPoseidonExtracted "poseidon_extracted.janno"
    , dataPoseidonDataMDS "mds.mds"
    ] -->
    [ dataPoseidonData "janno_final.RData" ]

  -- plots
  code04Paper "figure_1_temporal_and_spatial_distribution_of_input_data.R" %$
    [ dataPoseidonData "janno_final.RData"
    , dataSpatial "research_area.RData"
    , dataSpatial "extended_area.RData"
    , dataSpatial "epsg3035.RData"
    , dataSpatial "mobility_regions.RData"
    , dataPlotReferenceData "region_id_shapes.RData"
    , dataPlotReferenceData "age_colors_gradient.RData"
    ] -->
    [ plots "figure_1_temporal_and_spatial_distribution_of_input_data.pdf" ]