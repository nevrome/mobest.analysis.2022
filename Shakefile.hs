#!/usr/bin/env stack
-- stack --resolver lts-18.0 script --package shake

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

code x = "code" </> x
code01 x = code "01_poseidon_data_preparation" </> x
code04Paper x = code "04_plot_scripts" </> "paper" </> x
_data x = "data" </> x
dataSpatial x = _data "spatial" </> x
dataPlotReferenceData x = _data "plot_reference_data" </> x
dataPoseidonData x = _data "poseidon_data" </> x
dataPoseidonDataPoseidonExtracted x = dataPoseidonData "poseidon_extracted" </> x
dataPoseidonDataMDS x = dataPoseidonData "mds" </> x

rscript = cmd_ "./singularity_mobest.sif Rscript"
bashscript = cmd_ "./singularity_mobest.sif"

main :: IO ()
main = shakeArgs shakeOptions {
        shakeFiles = "_build", 
        shakeProgress = progressSimple
        } $ do
    want $ [
          "plots" </> "figure_1_temporal_and_spatial_distribution_of_input_data.jpeg"
        ]
    
    "plots" </> "figure_1_temporal_and_spatial_distribution_of_input_data.jpeg" %> \out -> do
        let script = code04Paper "figure_1_temporal_and_spatial_distribution_of_input_data.R"
            dataFiles = [
                  dataPoseidonData "janno_final.RData"
                , dataSpatial "research_area.RData"
                , dataSpatial "extended_area.RData"
                , dataSpatial "epsg3035.RData"
                , dataSpatial "mobility_regions.RData"
                , dataPlotReferenceData "region_id_shapes.RData"
                , dataPlotReferenceData "age_colors_gradient.RData"
              ]
        need $ script : dataFiles
        rscript script

    dataPoseidonData "janno_final.RData" %> \out -> do
        let script = code04Paper "figure_1_temporal_and_spatial_distribution_of_input_data.R"
            dataFiles = [
                  dataSpatial "epsg3035.RData"
                , dataSpatial "mobility_regions.RData"
                , dataPoseidonDataPoseidonExtracted "poseidon_extracted.janno"
                , dataPoseidonDataMDS "mds.mds"
              ]
        need $ script : dataFiles
        rscript script

    map dataSpatial [
          "research_area.RData"
        , "extended_area.RData"
        , "epsg3035.RData"
        , "mobility_regions.RData"
        ] &%> \out -> do
        let script = code01 "00_prepare_spatial_data.R"
            dataFiles = map ("data_tracked" </>) [
                  "research_area/research_area.gpkg"
                , "natural_earth_geodata/land_outline.RData"
                , "natural_earth_geodata/rivers.RData"
                , "natural_earth_geodata/lakes.RData"
                , "research_area/research_area_search.gpkg"
                , "mobility_regions/mobility_regions.gpkg"
                ]
        need $ script : dataFiles
        rscript script

    dataPoseidonData "poseidon_extracted.janno" %> \out -> do
        let script = code04Paper "figure_1_temporal_and_spatial_distribution_of_input_data.R"
            dataFiles = [
                  dataSpatial "epsg3035.RData"
                , dataSpatial "mobility_regions.RData"
                , dataPoseidonDataPoseidonExtracted "poseidon_extracted.janno"
                , dataPoseidonDataMDS "mds.mds"
              ]
        need $ script : dataFiles
        rscript script

    map dataPlotReferenceData [
          "region_id_shapes.RData"
        , "age_colors_gradient.RData"
        ] &%> \out -> do
        let script = code01 "00_prepare_plot_reference_data.R"
        need [script]
        rscript script

        

