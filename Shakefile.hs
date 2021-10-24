#!/usr/bin/env stack
-- stack --resolver lts-18.0 script --package shake,filepath

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.FilePath (takeExtension)

code x = "code" </> x
code01 x = code "01_poseidon_data_preparation" </> x
code04Paper x = code "04_plot_scripts" </> "paper" </> x
_data x = "data" </> x
dataSpatial x = _data "spatial" </> x
dataPlotReferenceData x = _data "plot_reference_data" </> x
dataPoseidonData x = _data "poseidon_data" </> x
dataPoseidonDataAADRv50 x = dataPoseidonData "aadrv50" </> x
dataPoseidonDataAADRv50AADRPoseidon x = dataPoseidonDataAADRv50 "aadr_poseidon" </> x
dataPoseidonDataPoseidonExtractedPreIdenticalsFilter x = dataPoseidonData "poseidon_extracted_pre_identicals_filter" </> x
dataPoseidonDataIdenticalFilter x = dataPoseidonData "identical_filter" </> x
dataPoseidonDataPoseidonExtracted x = dataPoseidonData "poseidon_extracted" </> x
dataPoseidonDataMDS x = dataPoseidonData "mds" </> x

relevantRunCommand :: FilePath -> Action ()
relevantRunCommand x
  | takeExtension x == ".R" = cmd_ "./singularity_mobest.sif Rscript" x
  | takeExtension x == ".sh" = cmd_ "./singularity_mobest.sif" x
  
process :: FilePath -> ([FilePath], [FilePath]) -> Rules ()
process script (input, output) =
      output &%> \out -> do
        need $ script : input
        relevantRunCommand script

main :: IO ()
main = shakeArgs shakeOptions {
        shakeFiles = "_build", 
        shakeProgress = progressSimple
        } $ do

    want [
           "plots" </> "figure_1_temporal_and_spatial_distribution_of_input_data.jpeg"
         ]
    
    -- #### poseidon data preparation #### --

    code01 "00_prepare_spatial_data.R" `process`
      ( map ("data_tracked" </>) [
          "research_area/research_area.gpkg"
        , "natural_earth_geodata/land_outline.RData"
        , "natural_earth_geodata/rivers.RData"
        , "natural_earth_geodata/lakes.RData"
        , "research_area/research_area_search.gpkg"
        , "mobility_regions/mobility_regions.gpkg"
        ] ,
        map dataSpatial [
          "research_area.RData"
        , "extended_area.RData"
        , "epsg3035.RData"
        , "mobility_regions.RData"
        ] )

    code01 "00_prepare_plot_reference_data.R" `process`
      ( [] ,
        map dataPlotReferenceData [
          "region_id_shapes.RData"
        , "age_colors_gradient.RData"
        ] )

    code01 "00_fetch_aadr.sh" `process`
      ( [] ,
        dataPoseidonDataAADRv50 "v50.0_1240k_public.anno" : map dataPoseidonDataAADRv50AADRPoseidon [
          "POSEIDON.yml"
        , "aadr_plink.geno"
        , "aadr_plink.snp"
        , "aadr_plink.ind"
        ] )

    code01 "01_janno_filter_for_relevant_individuals.R" `process`
      ( [ dataSpatial "epsg3035.RData"
        , dataSpatial "research_area.RData"
        , dataPoseidonDataAADRv50 "v50.0_1240k_public.anno"
        , code01 "00_aadr_age_string_parser.R"
        ] ,
        [ code01 "pre_identicals_filter_ind_list.txt"
        , dataPoseidonData "janno_pre_mds.RData"
        , dataPoseidonDataAADRv50AADRPoseidon "aadr_poseidon.janno"
        ] )

    code01 "02_pre_identicals_filter_poseidon_extract.sh" `process`
      ( map dataPoseidonDataAADRv50AADRPoseidon [
          "POSEIDON.yml"
        , "aadr_plink.geno"
        , "aadr_plink.snp"
        , "aadr_plink.ind"
        ] ,
        map dataPoseidonDataPoseidonExtractedPreIdenticalsFilter [
          "poseidon_extracted_pre_identicals_filter.bed"
        , "poseidon_extracted_pre_identicals_filter.bim"
        , "poseidon_extracted_pre_identicals_filter.fam"
        ] )

    code01 "03_distance_plink.sh" `process`
      ( code01 "myrange.txt" : map dataPoseidonDataPoseidonExtractedPreIdenticalsFilter [
          "poseidon_extracted_pre_identicals_filter.bed"
        , "poseidon_extracted_pre_identicals_filter.bim"
        , "poseidon_extracted_pre_identicals_filter.fam"
        ] ,
        map dataPoseidonDataIdenticalFilter [
            "plink.mdist"
          , "plink.mdist.id"
        ] )

    code01 "04_filter_by_genetic_distance.R" `process`
      ( [ dataPoseidonData "janno_pre_mds.RData"
        , dataPoseidonDataIdenticalFilter "plink.mdist"
        , dataPoseidonDataIdenticalFilter "plink.mdist.id"
        ] ,
        [ code01 "ind_list.txt" ] )

    code01 "05_poseidon_extract.sh" `process`
      ( code01 "pre_identicals_filter_ind_list.txt" : map dataPoseidonDataAADRv50AADRPoseidon [
          "POSEIDON.yml"
        , "aadr_plink.geno"
        , "aadr_plink.snp"
        , "aadr_plink.ind"
        , "aadr_poseidon.janno"
        ] ,
        map dataPoseidonDataPoseidonExtracted [
          "POSEIDON.yml"
        , "poseidon_extracted.bed"
        , "poseidon_extracted.bim"
        , "poseidon_extracted.fam"
        , "poseidon_extracted.janno"
        ] )

    code01 "06_mds_plink.sh" `process`
      ( code01 "myrange.txt" : map dataPoseidonDataPoseidonExtracted [
          "poseidon_extracted.bed"
        , "poseidon_extracted.bim"
        , "poseidon_extracted.fam"
        ] ,
        [ dataPoseidonDataMDS "mds.mds" ] )

    code01 "07_prepare_final_dataset.R" `process`
      ( [ dataSpatial "epsg3035.RData"
        , dataSpatial "mobility_regions.RData"
        , dataPoseidonDataPoseidonExtracted "poseidon_extracted.janno"
        , dataPoseidonDataMDS "mds.mds"
        ] ,
        [ dataPoseidonData "janno_final.RData" ] )

    -- #### plots #### --

    code04Paper "figure_1_temporal_and_spatial_distribution_of_input_data.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataSpatial "research_area.RData"
        , dataSpatial "extended_area.RData"
        , dataSpatial "epsg3035.RData"
        , dataSpatial "mobility_regions.RData"
        , dataPlotReferenceData "region_id_shapes.RData"
        , dataPlotReferenceData "age_colors_gradient.RData"
      ] ,
      [ "plots" </> "figure_1_temporal_and_spatial_distribution_of_input_data.jpeg" ] )
