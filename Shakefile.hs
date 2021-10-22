#!/usr/bin/env stack
-- stack --resolver lts-18.0 script --package shake
--{-# LANGUAGE BlockArguments,OverloadedStrings #-}

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

code :: FilePath -> FilePath
code x = "code" </> x

code01 :: FilePath -> FilePath
code01 x = code "01_poseidon_data_preparation" </> x

code04paper :: FilePath -> FilePath
code04paper x = code "04_plot_scripts" </> "paper" </> x

main :: IO ()
main = shakeArgs shakeOptions {
        shakeFiles = "_build", 
        shakeProgress = progressSimple
        } $ do
    want $ [
          "plots" </> "figure_1_temporal_and_spatial_distribution_of_input_data.jpeg"
        ]
    
    "plots" </> "figure_1_temporal_and_spatial_distribution_of_input_data.jpeg" %> \out -> do
        let script = code04paper "figure_1_temporal_and_spatial_distribution_of_input_data.R"
            dataFiles = map ("data" </>) [
                --   "poseidon_data" </> "janno_final.RData"
                "spatial" </> "research_area.RData"
                , "spatial" </> "extended_area.RData"
                , "spatial" </> "epsg3035.RData"
                -- , "plot_reference_data" </> "region_id_shapes.RData"
                -- , "plot_reference_data" </> "age_colors_gradient.RData"
                -- , "spatial" </> "mobility_regions.RData"
              ]
        need $ script : dataFiles
        cmd_ "Rscript" script

    map ("data/spatial" </>) [
          "research_area.RData"
        , "extended_area.RData"
        , "epsg3035.RData"
        ] &%> \[a,b,c] -> do
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
        cmd_ "Rscript" script

        

