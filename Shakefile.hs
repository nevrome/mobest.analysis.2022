#!/usr/bin/env stack
-- stack --resolver lts-18.17 script --package shake,filepath

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.FilePath (takeExtension)

-- #### settings #### --

data SettingsType = Local | Cluster

data Settings = Settings {
    setType :: SettingsType
  -- Path to the singularity image file
  -- required; create with "singularity_build_sif.sh"
  -- that's not part of the pipeline, because it requires sudo permissions
  , singularityContainer :: FilePath
  -- Path to mount into the singularity container
  -- https://sylabs.io/guides/3.0/user-guide/bind_paths_and_mounts.html
  , bindPath :: String
  -- How to run normal commands
  , qsubSmallCommand :: String
  , qsubMediumCommand :: String
  -- How to run SGE scripts
  , qsubScript :: String
}

localSettings = Settings {
    setType = Local
  , singularityContainer = "singularity_mobest.sif"
  , bindPath = ""
  , qsubSmallCommand = ""
  , qsubMediumCommand = ""
  , qsubScript = ""
}

mpiEVAClusterSettings = Settings {
    setType = Cluster
  , singularityContainer = "singularity_mobest.sif"
  , bindPath = "--bind=/mnt/archgen/users/schmid"
  --, qrsh = "qrsh -b y -cwd -q archgen.q -pe smp 4 -l h_vmem=16G -now n -V -N hedgehog"
  , qsubSmallCommand =  "qsub -sync y -b y -cwd -q archgen.q -pe smp 8 -l h_vmem=20G -now n -V -j y -o ~/log -N small"
  , qsubMediumCommand = "qsub -sync y -b y -cwd -q archgen.q -pe smp 16 -l h_vmem=32G -now n -V -j y -o ~/log -N medium"
  , qsubScript = "qsub -sync y -N large " -- trailing space is meaningful!
}

-- #### helper functions #### --

relevantRunCommand :: Settings -> FilePath -> Action ()
relevantRunCommand (Settings setType singularityContainer bindPath qsubSCommand qsubMCommand qsubScript) x
  | takeExtension x == ".R"   = cmd_ qsubSCommand "singularity" "exec" bindPath singularityContainer "Rscript" x
  | takeExtension x == ".Rq"  = cmd_ qsubMCommand "singularity" "exec" bindPath singularityContainer "Rscript" x
  | takeExtension x == ".sh"  = cmd_ qsubSCommand "singularity" "exec" bindPath singularityContainer x
  | takeExtension x == ".shq" =
    case setType of
      Local -> error "Can not run cluster scripts locally."
      Cluster -> cmd_ $ qsubScript ++ x

process :: FilePath -> ([FilePath], [FilePath]) -> Rules ()
process script (input, output) =
      let --settings = localSettings
          settings = mpiEVAClusterSettings
      in output &%> \out -> do
        need $ [script, singularityContainer settings] ++ input
        --need $ [script] ++ input
        relevantRunCommand settings script

-- #### set up file paths #### --

code x = "code" </> x
code01 x = code "01_poseidon_data_preparation" </> x
code02 x = code "02_parameter_estimation" </> x
code02Variogram x = code02 "01_variogram_experiments" </> x
code02Crossvalidation x = code02 "02_crossvalidation" </> x
code02MLE x = code02 "03_laGP_maximum_likelihood_estimation" </> x
code03 x = code "03_origin_search" </> x
code04Paper x = code "04_plot_scripts" </> "paper" </> x
code05 x = code "05_table_scripts" </> x
code06MDS3 x = code "06_alternative_parameter_exploration" </> "MDS_3_dimensions" </> x
code06Rearview x = code "06_alternative_parameter_exploration" </> "different_rearview_distances" </> x

_data x = "data" </> x
dataSpatial x = _data "spatial" </> x
dataPlotReferenceData x = _data "plot_reference_data" </> x
dataPoseidonData x = _data "poseidon_data" </> x
dataPoseidonDataAADRv50 x = dataPoseidonData "aadrv50" </> x
dataPoseidonDataPoseidonExtractedPreIdenticalsFilter x = dataPoseidonData "poseidon_extracted_pre_identicals_filter" </> x
dataPoseidonDataIdenticalFilter x = dataPoseidonData "identical_filter" </> x
dataPoseidonDataPoseidonExtracted x = dataPoseidonData "poseidon_extracted" </> x
dataPoseidonDataMDS x = dataPoseidonData "mds" </> x
dataParameterExploration x = _data "parameter_exploration" </> x
dataParameterExplorationVariogram x = dataParameterExploration "variogram" </> x
dataParameterExplorationCrossvalidation x = dataParameterExploration "crossvalidation" </> x
dataParameterExplorationMLE x = dataParameterExploration "mle" </> x
dataOriginSearch x = _data "origin_search" </> x
dataOriginSearchAROKS x = dataOriginSearch "age_resampling+one_kernel_setting" </> x
dataGPR x = _data "gpr" </> x

tables x = "tables" </> x
plots x = "plots" </> x

-- #### pipeline #### --

main :: IO ()
main = shakeArgs shakeOptions {
        --https://hackage.haskell.org/package/shake-0.19.6/docs/Development-Shake.html#g:5
          shakeFiles = "_build"
        , shakeProgress = progressSimple
        , shakeColor = True
        , shakeVerbosity = Verbose
        , shakeThreads = 10
        , shakeTimings = True
        , shakeChange = ChangeDigest
        } $ do

    want $ map plots [ 
          "figure_1_temporal_and_spatial_distribution_of_input_data.jpeg"
        , "figure_2_mds.jpeg"
        , "figure_3_interpolation_map_matrix.jpeg"
        , "figure_4_genetic_distance_example_maps.jpeg"
        , "figure_5_mobility_curves.jpeg"
        , "figure_sup_1_semivariogram.jpeg"
        , "figure_sup_2_semivariogram_space_time.jpeg"
        , "figure_sup_3_semivariogram_fitting.jpeg"
        , "figure_sup_4_semivariogram_nugget.jpeg"
        , "figure_sup_5_mle_anisotropic.jpeg"
        , "figure_sup_6_kernel_size_meaning.jpeg"
        , "figure_sup_7_mle_isotropic.jpeg"
        , "figure_sup_8_crossvalidation_prediction_accuracy.jpeg"
        , "figure_sup_9_crossvalidation_rasters.jpeg"
        , "figure_sup_10_rearview_distance.jpeg"
        , "figure_sup_11_timepillars.jpeg"
        , "figure_sup_12_direction_windrose_matrix.jpeg"
        , "figure_sup_13_distance_fraction_curves.jpeg"
        , "figure_sup_14_mds3.jpeg"
        , "figure_sup_15_distance_correlation_mds3.jpeg"
        , "figure_sup_16_semivariogram_nugget_mds3.jpeg"
        , "figure_sup_17_crossvalidation_rasters_mds3.jpeg"
        , "figure_sup_18_interpolation_map_matrix_mds3.jpeg"
        , "figure_sup_19_mobility_curves_mds3.jpeg"
        , "figure_sup_20_mobility_curves_retro_low.jpeg"
        , "figure_sup_21_mobility_curves_retro_high.jpeg"
        , "figure_sup_22_mobility_curve_comparison.jpeg"
        ] ++ 
        [ dataParameterExplorationCrossvalidation "interpol_comparison_1.RData" 
        , tables "table_sup_1_origin_search_table.csv"
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
        , "search_area.RData"
        ] )

    code01 "00_prepare_plot_reference_data.R" `process`
      ( [] ,
        map dataPlotReferenceData [
          "region_id_shapes.RData"
        , "age_colors_gradient.RData"
        ] )

    code01 "00_download_aadr.sh" `process`
      ( [] ,
        map dataPoseidonDataAADRv50 [
          "v50.0_1240k_public.anno"
        , "aadr_eig.geno"
        , "aadr_eig.snp"
        , "aadr_eig.ind"
        ] )

    code01 "01_janno_filter_for_relevant_individuals.R" `process`
      ( [ dataSpatial "epsg3035.RData"
        , dataSpatial "research_area.RData"
        , dataPoseidonDataAADRv50 "v50.0_1240k_public.anno"
        , dataPoseidonDataAADRv50 "aadr_eig.geno"
        , dataPoseidonDataAADRv50 "aadr_eig.geno"
        , dataPoseidonDataAADRv50 "aadr_eig.geno"
        , code01 "00_aadr_age_string_parser.R"
        ] ,
        [ code01 "pre_identicals_filter_ind_list.txt"
        , dataPoseidonData "janno_pre_mds.RData"
        , dataPoseidonDataAADRv50 "aadr_poseidon.janno"
        , dataPoseidonDataAADRv50 "POSEIDON.yml"
        ] )

    code01 "02_pre_identicals_filter_poseidon_extract.sh" `process`
      ( code01 "pre_identicals_filter_ind_list.txt" : map dataPoseidonDataAADRv50 [
          "POSEIDON.yml"
        , "aadr_poseidon.janno"
        , "aadr_eig.geno"
        , "aadr_eig.snp"
        , "aadr_eig.ind"
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
      ( code01 "ind_list.txt" : map dataPoseidonDataAADRv50 [
          "POSEIDON.yml"
        , "aadr_eig.geno"
        , "aadr_eig.snp"
        , "aadr_eig.ind"
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

    -- #### parameter estimation #### --

    code02Variogram "01_variogram_calculation.R" `process`
      ( [ dataPoseidonData "janno_final.RData" 
        ] ,
        map dataParameterExplorationVariogram [
          "all_distances.RData"
        , "binned_distances.RData"
        ]
      )

    code02Variogram "02_nugget_estimation.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataParameterExplorationVariogram "all_distances.RData"
        ] ,
        map dataParameterExplorationVariogram [
          "lower_left_variogram.RData"
        , "estimated_nuggets.RData"
        , "nuggets.txt"
        ]
      )
    
    code02Crossvalidation "sge_parameter_exploration.shq" `process`
      ( [ code02Crossvalidation "crossvalidation.R"
        , dataPoseidonData "janno_final.RData"
        , dataParameterExplorationVariogram "nuggets.txt"
        ] , 
        [ dataParameterExplorationCrossvalidation "interpol_comparison_1.RData" ] )

    code02Crossvalidation "modify_crossvalidation_results.R" `process`
      ( [ dataParameterExplorationCrossvalidation "interpol_comparison_1.RData" ] ,
        map dataParameterExplorationCrossvalidation [
          "interpol_comparison.RData"
        , "interpol_comparison_group.RData"
        , "best_kernel.RData"
        ] )

    code02MLE "anisotropic_mle.Rq" `process`
        ( [ dataPoseidonData "janno_final.RData" ] ,
          [ dataParameterExplorationMLE "mlesep_out.RData" ] )

    code02MLE "isotropic_mle.Rq" `process`
        ( [ dataPoseidonData "janno_final.RData"
          , dataParameterExplorationVariogram "nuggets.txt"
          ] ,
          [ dataParameterExplorationMLE "mle_out.RData" ] )

    -- #### origin search #### --

    code03 "00_interpolation_and_origin_search_settings.R" `process`
      ( [ dataParameterExplorationCrossvalidation "best_kernel.RData" ] ,
        map dataOriginSearch [
          "default_kernel.RData"
        , "kernel_theta_data.RData"
        , "retrospection_distance.RData"
        ] )

    code03 "01_interpolation_for_selected_timeslices.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataSpatial "extended_area.RData"
        , dataOriginSearch "default_kernel.RData"
        ] ,
        [ dataGPR "interpol_grid_median_selected_timeslices.RData" ] )

    code03 "02_interpolation_at_specific_places_median_age+one_kernel_setting.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataSpatial "epsg3035.RData"
        , dataOriginSearch "default_kernel.RData"
        ] ,
        [ dataGPR "interpol_grid_examples.RData" ] )

    code03 "03_interpolation_and_search_for_selected_individuals.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataSpatial "search_area.RData"
        , dataSpatial "epsg3035.RData"
        , dataOriginSearch "default_kernel.RData"
        , dataOriginSearch "retrospection_distance.RData"
        ] ,
        map dataOriginSearch [
          "janno_search.RData"
        , "closest_points_examples.RData"
        , "distance_grid_examples.RData"
        ] )

    code03 "05b_sge_origin_search.shq" `process`
      ( [ code03 "05a_origin_search_pipeline-age_resampling+one_kernel_setting.R"
        , dataPoseidonData "janno_final.RData"
        , dataSpatial "search_area.RData"
        , dataOriginSearch "default_kernel.RData"
        , dataOriginSearch "retrospection_distance.RData"
        ] ,
        [ dataOriginSearchAROKS "run_1.RData" ] )

    code03 "06_origin_search_merge_and_prep.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataOriginSearchAROKS "run_1.RData"
        ] ,
        map dataOriginSearch [
          "origin_grid_modified.RData"
        , "origin_grid_mean.RData"
        , "moving_origin_grid.RData"
        , "no_data_windows.RData"
        ] )

    -- #### alternative parameter exploration: MDS3 ####

    code06MDS3 "01_interpolation_and_origin_search_settings.R" `process`
      ( [ dataParameterExplorationCrossvalidation "best_kernel.RData" ] ,
        map dataOriginSearch [
          "default_kernel_mds3.RData"
        , "kernel_theta_data_mds3.RData"
        , "retrospection_distance_mds3.RData"
        ] )

    code06MDS3 "02_interpolation_for_selected_timeslices.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataSpatial "extended_area.RData"
        , dataOriginSearch "default_kernel_mds3.RData"
        ] ,
        [ dataGPR "interpol_grid_median_selected_timeslices_mds3.RData" ] )

    code06MDS3 "03b_sge_origin_search.shq" `process`
      ( [ code06MDS3 "03a_origin_search_pipeline-age_resampling+one_kernel_setting.R"
        , dataPoseidonData "janno_final.RData"
        , dataSpatial "search_area.RData"
        , dataOriginSearch "default_kernel_mds3.RData"
        , dataOriginSearch "retrospection_distance_mds3.RData"
        ] ,
        [ dataOriginSearchAROKS "mds3_run_1.RData" ] )

    code06MDS3 "04_origin_search_merge_and_prep.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataOriginSearchAROKS "mds3_run_1.RData"
        ] ,
        map dataOriginSearch [
          "origin_grid_modified_mds3.RData"
        , "origin_grid_mean_mds3.RData"
        , "moving_origin_grid_mds3.RData"
        , "no_data_windows_mds3.RData"
        ] )

    -- #### alternative parameter exploration: different rearview distances ####

    code06Rearview "01_interpolation_and_origin_search_settings.R" `process`
      ( [ dataOriginSearch "default_kernel.RData" ] ,
        [ dataOriginSearch "retrospection_distance_retrovar.RData" ] )

    code06Rearview "02b_sge_origin_search.shq" `process`
      ( [ code06Rearview "02a_origin_search_pipeline-age_resampling+one_kernel_setting.R"
        , dataPoseidonData "janno_final.RData"
        , dataSpatial "search_area.RData"
        , dataOriginSearch "default_kernel.RData"
        , dataOriginSearch "retrospection_distance_retrovar.RData"
        ] ,
        [ dataOriginSearchAROKS "retrovar_run_1.RData" ] )

    code06Rearview "03_origin_search_merge_and_prep.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataOriginSearchAROKS "retrovar_run_1.RData"
        , dataOriginSearch "retrospection_distance_retrovar.RData"
        ] ,
        map dataOriginSearch [
          "origin_grid_derived_data_retro_low.RData"
        , "origin_grid_derived_data_retro_high.RData"
        ] )

    -- #### tables #### --

    code05 "table_sup_1_origin_per_individual_table.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
      , dataOriginSearch "origin_grid_modified.RData"
      ] ,
      [ tables "table_sup_1_origin_search_table.csv" ] )

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
        [ plots "figure_1_temporal_and_spatial_distribution_of_input_data.jpeg" ] )

    code04Paper "figure_2_mds.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataPlotReferenceData "region_id_shapes.RData"
        , dataPlotReferenceData "age_colors_gradient.RData"
        ] ,
        [ plots "figure_2_mds.jpeg" ] )

    code04Paper "figure_3_interpolation_map_matrix.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataSpatial "extended_area.RData"
        , dataSpatial "epsg3035.RData"
        , dataGPR "interpol_grid_median_selected_timeslices.RData"
        ] ,
        [ plots "figure_3_interpolation_map_matrix.jpeg" ] )

    code04Paper "figure_4_genetic_distance_example_maps.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataSpatial "research_area.RData"
        , dataSpatial "extended_area.RData"
        , dataSpatial "epsg3035.RData"
        ] ++ map dataOriginSearch [
          "janno_search.RData"
        , "closest_points_examples.RData"
        , "distance_grid_examples.RData"
        ],
        [ plots "figure_4_genetic_distance_example_maps.jpeg" ] )

    code04Paper "figure_5_mobility_curves.R" `process`
      ( [ code04Paper "individuals_to_highlight.R"
        , code04Paper "mobility_curves_plot_function.R"
        , dataPoseidonData "janno_final.RData"
        ] ++ map dataOriginSearch [
          "origin_grid_mean.RData"
        , "moving_origin_grid.RData"
        , "no_data_windows.RData"
        ],
        [ plots "figure_5_mobility_curves.jpeg" ] )

    code04Paper "figure_sup_1_semivariogram.R" `process`
      ( [ dataParameterExplorationVariogram "binned_distances.RData" ] ,
        [ plots "figure_sup_1_semivariogram.jpeg" ] )

    code04Paper "figure_sup_2_semivariogram_space_time.R" `process`
      ( [ dataParameterExplorationVariogram "all_distances.RData" ] ,
        [ plots "figure_sup_2_semivariogram_space_time.jpeg" ] )

    code04Paper "figure_sup_3_semivariogram_fitting.R" `process`
      ( [ dataParameterExplorationVariogram "binned_distances.RData" ] ,
        [ plots "figure_sup_3_semivariogram_fitting.jpeg" ] )

    code04Paper "figure_sup_4_semivariogram_nugget.R" `process`
      ( map dataParameterExplorationVariogram [
          "lower_left_variogram.RData"
        , "estimated_nuggets.RData"
        ] ,
        [ plots "figure_sup_4_semivariogram_nugget.jpeg" ] )
    
    code04Paper "figure_sup_5_mle_anisotropic.R" `process`
      ( [ dataParameterExplorationMLE "mlesep_out.RData" ] ,
        [ plots "figure_sup_5_mle_anisotropic.jpeg" ] )

    code04Paper "figure_sup_7_mle_isotropic.R" `process`
      ( [ dataParameterExplorationMLE "mle_out.RData" ] ,
        [ plots "figure_sup_7_mle_isotropic.jpeg" ] )

    code04Paper "figure_sup_6_kernel_size_meaning.R" `process`
      ( [ ] ,
        [ plots "figure_sup_6_kernel_size_meaning.jpeg" ] )

    code04Paper "figure_sup_8_crossvalidation_prediction_accuracy.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataParameterExplorationCrossvalidation "interpol_comparison.RData"
        ] ,
        [ plots "figure_sup_8_crossvalidation_prediction_accuracy.jpeg" ] )

    code04Paper "figure_sup_9_crossvalidation_rasters.R" `process`
      ( map dataParameterExplorationCrossvalidation [
          "interpol_comparison_group.RData"
        , "best_kernel.RData"
        ] ,
        [ plots "figure_sup_9_crossvalidation_rasters.jpeg" ] )

    code04Paper "figure_sup_10_rearview_distance.R" `process`
      ( map dataOriginSearch [ 
          "kernel_theta_data.RData"
        , "kernel_theta_data_mds3.RData"
        ] ,
        [ plots "figure_sup_10_rearview_distance.jpeg" ] )

    code04Paper "figure_sup_11_timepillars.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataPlotReferenceData "age_colors_gradient.RData"
        , dataGPR "interpol_grid_examples.RData"
        ] ,
        [ plots "figure_sup_11_timepillars.jpeg" ] )

    code04Paper "figure_sup_12_mean_direction_windrose_matrix.R" `process`
      ( [ dataOriginSearch "origin_grid_modified.RData" ] ,
        [ plots "figure_sup_12_direction_windrose_matrix.jpeg" ] )

    code04Paper "figure_sup_13_distance_fraction_curves.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataOriginSearch "moving_origin_grid.RData" ] ,
        [ plots "figure_sup_13_distance_fraction_curves.jpeg" ] )

    code04Paper "figure_sup_14_mds_mds3.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataPlotReferenceData "region_id_shapes.RData"
        , dataPlotReferenceData "age_colors_gradient.RData" ] ,
        [ plots "figure_sup_14_mds3.jpeg" ] )

    code04Paper "figure_sup_15_distance_correlation_mds3.R" `process`
      ( [ dataParameterExplorationVariogram "all_distances.RData" ] ,
        [ plots "figure_sup_15_distance_correlation_mds3.jpeg" ] )

    code04Paper "figure_sup_16_semivariogram_nugget_mds3.R" `process`
      ( map dataParameterExplorationVariogram [
          "lower_left_variogram.RData"
        , "estimated_nuggets.RData"
        ] ,
        [ plots "figure_sup_16_semivariogram_nugget_mds3.jpeg" ] )

    code04Paper "figure_sup_17_crossvalidation_rasters_mds3.R" `process`
      ( map dataParameterExplorationCrossvalidation [
          "interpol_comparison_group.RData"
        , "best_kernel.RData"
        ] ,
        [ plots "figure_sup_17_crossvalidation_rasters_mds3.jpeg" ] )

    code04Paper "figure_sup_18_interpolation_map_matrix_mds3.R" `process`
      ( [ dataPoseidonData "janno_final.RData"
        , dataSpatial "extended_area.RData"
        , dataSpatial "epsg3035.RData"
        , dataGPR "interpol_grid_median_selected_timeslices_mds3.RData"
        ] ,
        [ plots "figure_sup_18_interpolation_map_matrix_mds3.jpeg" ] )

    code04Paper "figure_sup_19_mobility_curves_mds3.R" `process`
      ( [ code04Paper "individuals_to_highlight.R"
        , code04Paper "mobility_curves_plot_function.R"
        , dataPoseidonData "janno_final.RData"
        ] ++ map dataOriginSearch [
          "origin_grid_mean_mds3.RData"
        , "moving_origin_grid_mds3.RData"
        , "no_data_windows_mds3.RData"
        ],
        [ plots "figure_sup_19_mobility_curves_mds3.jpeg" ] )

    code04Paper "figure_sup_20_mobility_curves_retro_low.R" `process`
      ( [ code04Paper "individuals_to_highlight.R"
        , code04Paper "mobility_curves_plot_function.R"
        , dataPoseidonData "janno_final.RData"
        , dataOriginSearch "origin_grid_derived_data_retro_low.RData"
        ],
        [ plots "figure_sup_20_mobility_curves_retro_low.jpeg" ] )

    code04Paper "figure_sup_21_mobility_curves_retro_high.R" `process`
      ( [ code04Paper "individuals_to_highlight.R"
        , code04Paper "mobility_curves_plot_function.R"
        , dataPoseidonData "janno_final.RData"
        , dataOriginSearch "origin_grid_derived_data_retro_high.RData"
        ],
        [ plots "figure_sup_21_mobility_curves_retro_high.jpeg" ] )

    code04Paper "figure_sup_22_mobility_curve_comparison.R" `process`
      ( map dataOriginSearch [  
          "origin_grid_derived_data_retro_high.RData"
        , "origin_grid_derived_data_retro_low.RData"
        , "origin_grid_mean_mds3.RData"
        , "origin_grid_mean.RData"
        ],
        [ plots "figure_sup_22_mobility_curve_comparison.jpeg" ] )
