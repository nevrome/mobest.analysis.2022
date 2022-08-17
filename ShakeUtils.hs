module ShakeUtils where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.FilePath (takeExtension)

-- #### settings #### --

myShakeOpts = shakeOptions {
    --https://hackage.haskell.org/package/shake-0.19.6/docs/Development-Shake.html#g:5
      shakeFiles     = "_build"
    , shakeThreads   = 10
    , shakeChange    = ChangeModtimeAndDigest --ChangeDigest
    , shakeProgress  = progressSimple
    , shakeColor     = True
    , shakeReport    = ["ShakeReport.html"]
    , shakeVerbosity = Verbose
    , shakeTimings   = True
    , shakeStaunch   = True --  Operate in staunch mode, where building continues even after errors, similar to make --keep-going.
    }

data Settings = Settings {
  -- Path to the singularity image file
  -- required; create with "singularity_build_sif.sh"
  -- that's not part of the pipeline, because it requires sudo permissions
    singularityContainer :: FilePath
  -- Path to mount into the singularity container
  -- https://sylabs.io/guides/3.0/user-guide/bind_paths_and_mounts.html
  , bindPath :: String
  -- How to run normal commands
  , qsubSmallCommand :: String
  , qsubLargeMemoryCommand :: String
  , qsubMediumCommand :: String
  , qsubSmartSNP :: String
  , qsubEMU :: String
  , qsubPLINK :: String
  , qsubRC8M50 :: String
  , qsubRC48M50 :: String
  -- How to run SGE scripts
  , qsubScript :: String
}

mpiEVAClusterSettings = Settings {
    singularityContainer   = "singularity_mobest.sif"
  , bindPath               = "--bind=/mnt/archgen/users/schmid"
  , qsubSmallCommand       = "qsub -sync y -b y -cwd -q archgen.q -pe smp 8  -l h_vmem=20G -now n -V -j y -o ~/log -N small"
  , qsubLargeMemoryCommand = "qsub -sync y -b y -cwd -q archgen.q -pe smp 8  -l h_vmem=50G -now n -V -j y -o ~/log -N lmemory"
  , qsubMediumCommand      = "qsub -sync y -b y -cwd -q archgen.q -pe smp 16 -l h_vmem=32G -now n -V -j y -o ~/log -N medium"
  , qsubSmartSNP           = "qsub -sync y -b y -cwd -q archgen.q -pe smp 8  -l h_vmem=200G -now n -V -j y -o ~/log -N smartsnp"
  , qsubEMU                = "qsub -sync y -b y -cwd -q archgen.q -pe smp 48 -l h_vmem=100G -now n -V -j y -o ~/log -N emu"
  , qsubPLINK              = "qsub -sync y -b y -cwd -q archgen.q -pe smp 8  -l h_vmem=100G -now n -V -j y -o ~/log -N plink"
  , qsubRC8M50             = "qsub -sync y -b y -cwd -q archgen.q -pe smp 8  -l h_vmem=50G -now n -V -j y -o ~/log -N RC8M50"
  , qsubRC48M50            = "qsub -sync y -b y -cwd -q archgen.q -pe smp 48 -l h_vmem=50G -now n -V -j y -o ~/log -N RC48M50"
  , qsubScript             = "qsub -sync y -N large " -- trailing space is meaningful!
}

-- #### helper functions #### --

relevantRunCommand :: Settings -> FilePath -> Action ()
relevantRunCommand (Settings singularityContainer bindPath s lm m smartSNP emu plink rc8m50 rc48m50 qsubScript) x
  | takeExtension x == ".R"         = cmd_ s "singularity" "exec" bindPath singularityContainer "Rscript" x
  | takeExtension x == ".Rq"        = cmd_ m "singularity" "exec" bindPath singularityContainer "Rscript" x
  | takeExtension x == ".shlm"      = cmd_ lm "singularity" "exec" bindPath singularityContainer x
  | takeExtension x == ".sh"        = cmd_ s "singularity" "exec" bindPath singularityContainer x
  | takeExtension x == ".Rsmartsnp" = cmd_ smartSNP "singularity" "exec" bindPath singularityContainer "Rscript" x
  | takeExtension x == ".shemu"     = cmd_ emu "singularity" "exec" bindPath singularityContainer x
  | takeExtension x == ".shplink"   = cmd_ plink "singularity" "exec" bindPath singularityContainer x
  | takeExtension x == ".RC8M50"    = cmd_ rc8m50 "singularity" "exec" bindPath singularityContainer "Rscript" x
  | takeExtension x == ".RC48M50"   = cmd_ rc48m50 "singularity" "exec" bindPath singularityContainer "Rscript" x
  | takeExtension x == ".shq"       = cmd_ $ qsubScript ++ x
  | otherwise = error $ "undefined file extension: " ++ x

infixl 3 %$
(%$) :: FilePath -> ([FilePath], [FilePath]) -> Rules ()
(%$) script (input, output) =
  let settings = mpiEVAClusterSettings
  in output &%> \out -> do
    need $ [script, singularityContainer settings] ++ input
    relevantRunCommand settings script

infixl 4 -->
(-->) :: a -> b -> (a,b)
(-->) x y = (x,y)

-- #### set up file paths #### --

-- code dirs
code x = "code" </> x
code01 x = code "01_data_preparation" </> x
code0101 x = code01 "01_acquire_input_data" </> x
code0102 x = code01 "02_initial_sample_selection" </> x
code0103 x = code01 "03_remove_related_individuals" </> x
code0104 x = code01 "04_prepare_snp_selections" </> x
code0105 x = code01 "05_run_multivariate_analysis" </> x
code02 x = code "02_parameter_estimation" </> x
code03 x = code "03_origin_search" </> x
code04Paper x = code "04_plot_scripts" </> "paper" </> x
code07 x = code "07_simulation" </> x

-- probably outdated
code05 x = code "05_table_scripts" </> x
code06MDS3 x = code "06_alternative_parameter_exploration" </> "MDS_3_dimensions" </> x
code06Rearview x = code "06_alternative_parameter_exploration" </> "different_rearview_distances" </> x

-- general data dirs
dataTracked x = "data_tracked" </> x
_data x = "data" </> x
dataSpatial x = _data "spatial" </> x
dataPlotReferenceData x = _data "plot_reference_data" </> x

-- data download dirs
dataGeno x = _data "genotype_data" </> x
dataGenoAADRv501240K x = dataGeno "aadrv50_1240K" </> x
dataGenoAADRv501240KHO x = dataGeno "aadrv50_1240K_HO" </> x
dataGenoInitialSelection x = dataGeno "initial_selection" </> x
dataGenoRemoveRelatedIndividuals x = dataGeno "remove_related_individuals" </> x
dataGenoRemoveRelatedIndividualsSelection x = dataGenoRemoveRelatedIndividuals "remove_related_selection" </> x

-- data filter dirs
dataGenoSnpSub x = dataGeno "snp_subsets" </> x 
dataGenoSnpSubUnfilteredPre x = dataGenoSnpSub "unfiltered_snp_selection_pre_ind_correction" </> x
dataGenoSnpSubFilteredPre x = dataGenoSnpSub "filtered_snp_selection_pre_ind_correction" </> x
dataGenoSnpSubUnfiltered x = dataGenoSnpSub "unfiltered_snp_selection" </> x
dataGenoSnpSubFiltered x = dataGenoSnpSub "filtered_snp_selection" </> x
dataGenoAADRModern x = dataGeno "aadrv50_1240K_HO_Western_Eurasia_modern" </> x
dataGenoSnpSubUnfilteredModern x = dataGenoSnpSub "unfiltered_snp_selection_with_modern_reference_pops" </> x
dataGenoSnpSubFilteredModern x = dataGenoSnpSub "filtered_snp_selection_with_modern_reference_pops" </> x

-- multivar analysis dirs
dataGenoMultivar x = dataGeno "multivariate_analysis" </> x
dataGenoMultivarMDSUnfiltered x = dataGenoMultivar "MDS_unfiltered_snp_selection" </> x
dataGenoMultivarMDSFiltered x = dataGenoMultivar "MDS_filtered_snp_selection" </> x
dataGenoMultivarPCAUnfiltered x = dataGenoMultivar "PCA_unfiltered_snp_selection" </> x
dataGenoMultivarPCAFiltered x = dataGenoMultivar "PCA_filtered_snp_selection" </> x
dataGenoMultivarEMUUnfiltered x = dataGenoMultivar "EMU_unfiltered_snp_selection" </> x
dataGenoMultivarEMUFiltered x = dataGenoMultivar "EMU_filtered_snp_selection" </> x
dataGenoMultivarPCAProjUnfiltered x = dataGenoMultivar "PCA_projected_unfiltered_snp_selection" </> x
dataGenoMultivarPCAProjFiltered x = dataGenoMultivar "PCA_projected_filtered_snp_selection" </> x

-- param exploration dirs
dataParamExp x = _data "parameter_exploration" </> x
dataParamExpCrossval x = dataParamExp "crossvalidation" </> x
dataParamExpTargeted x = dataParamExp "targeted" </> x

-- origin search dirs
dataOriginSearch x =  _data "origin_search" </> x
dataOriginSearchLarge x = dataOriginSearch "large_origin_search" </> x

-- simulation dirs
dataSimulation x = _data "simulation" </> x

-- dataPoseidonDataPoseidonExtractedPreIdenticalsFilter x = dataPoseidonData "poseidon_extracted_pre_identicals_filter" </> x
-- dataPoseidonDataIdenticalFilter x = dataPoseidonData "identical_filter" </> x
-- dataPoseidonDataPoseidonExtracted x = dataPoseidonData "poseidon_extracted" </> x
-- dataPoseidonDataMDS x = dataPoseidonData "mds" </> x
-- dataParameterExplorationVariogram x = dataParameterExploration "variogram" </> x
-- dataParameterExplorationCrossvalidation x = dataParameterExploration "crossvalidation" </> x
-- dataParameterExplorationMLE x = dataParameterExploration "mle" </> x
-- dataOriginSearch x = _data "origin_search" </> x
-- dataOriginSearchAROKS x = dataOriginSearch "age_resampling+one_kernel_setting" </> x
-- dataGPR x = _data "gpr" </> x

-- table and figure output dirs
tables x = "tables" </> x
plots x = "plots" </> x
figures x = "figures" </> x
