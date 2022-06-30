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
    , shakeChange    = ChangeDigest
    , shakeProgress  = progressSimple
    , shakeColor     = False
    , shakeReport    = ["ShakeReport.html"]
    , shakeVerbosity = Verbose
    , shakeTimings   = True
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
  -- How to run SGE scripts
  , qsubScript :: String
}

mpiEVAClusterSettings = Settings {
    singularityContainer   = "singularity_mobest.sif"
  , bindPath               = "" -- "--bind=/mnt/archgen/users/schmid"
  , qsubSmallCommand       = "" -- "qsub -sync y -b y -cwd -q archgen.q -pe smp 8  -l h_vmem=20G -now n -V -j y -o ~/log -N small"
  , qsubLargeMemoryCommand = "qsub -sync y -b y -cwd -q archgen.q -pe smp 8  -l h_vmem=40G -now n -V -j y -o ~/log -N lmemory"
  , qsubMediumCommand      = "qsub -sync y -b y -cwd -q archgen.q -pe smp 16 -l h_vmem=32G -now n -V -j y -o ~/log -N medium"
  , qsubScript             = "qsub -sync y -N large " -- trailing space is meaningful!
}

-- #### helper functions #### --

relevantRunCommand :: Settings -> FilePath -> Action ()
relevantRunCommand (Settings singularityContainer bindPath qsubSCommand qsubLMCommand qsubMCommand qsubScript) x
  | takeExtension x == ".R"    = cmd_ qsubSCommand "singularity" "exec" bindPath singularityContainer "Rscript" x
  | takeExtension x == ".Rq"   = cmd_ qsubMCommand "singularity" "exec" bindPath singularityContainer "Rscript" x
  | takeExtension x == ".shlm" = cmd_ qsubLMCommand "singularity" "exec" bindPath singularityContainer x
  | takeExtension x == ".sh"   = cmd_ qsubSCommand "singularity" "exec" bindPath singularityContainer x
  | takeExtension x == ".shq"  = cmd_ $ qsubScript ++ x

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

code x = "code" </> x
code01 x = code "01_data_preparation" </> x
code0101 x = code01 "01_acquire_input_data" </> x
code02 x = code "02_parameter_estimation" </> x
code02Variogram x = code02 "01_variogram_experiments" </> x
code02Crossvalidation x = code02 "02_crossvalidation" </> x
code02MLE x = code02 "03_laGP_maximum_likelihood_estimation" </> x
code03 x = code "03_origin_search" </> x
code04Paper x = code "04_plot_scripts" </> "paper" </> x
code05 x = code "05_table_scripts" </> x
code06MDS3 x = code "06_alternative_parameter_exploration" </> "MDS_3_dimensions" </> x
code06Rearview x = code "06_alternative_parameter_exploration" </> "different_rearview_distances" </> x

data_tracked x = "data_tracked" </> x

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
figures x = "figures" </> x
