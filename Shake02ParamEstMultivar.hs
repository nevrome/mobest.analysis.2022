#!/usr/bin/env stack
{- stack script
 --resolver lts-18.28
 --package shake,filepath
 -}

import ShakeUtils
import Development.Shake

main :: IO ()
main = shakeArgs myShakeOpts $ do

  want $ [
      dataParamExp "distance_products.RData"
    , dataParamExp "crossvalidation_kernel_comparison.RData"
    , dataParamExp "crossvalidation_best_kernels.RData"
    , dataParamExp "crossvalidation_multivar_comparison.RData"
    ]

  code02 "01b_distance_correlation.RC8M50" %$
    [ dataGeno "janno_final.RData"
    , dataGeno "multivar_perm_obs_bundles.RData"
    , code02 "01a_distance_helper_functions.R"
    ] -->
    [ dataParamExp "distance_products.RData" ]

  code02 "02b_sge_crossvalidation.shq" %$
    [ code02 "02a_crossvalidation.R"
    , dataGeno "janno_final.RData"
    , dataGeno "multivar_perm_obs_bundles.RData"
    , dataParamExp "distance_products.RData"
    ] -->
    [ dataParamExpCrossval "interpol_comparison_C1_mds_u_1.RData" ]

  code02 "02c_modify_crossvalidation_results.R" %$
    [ code02 "01a_distance_helper_functions.R"
    , dataParamExp "distance_products.RData"
    , dataParamExpCrossval "interpol_comparison_C1_mds_u_1.RData"
    , code02 "01a_distance_helper_functions.R"
    ] -->
    [ dataParamExp "crossvalidation_kernel_comparison.RData"
    , dataParamExp "crossvalidation_best_kernels.RData"
    , dataParamExp "crossvalidation_multivar_comparison.RData"
    ]

  --code02 "03a_variogram_calculation.R" %$
  --  [ dataGeno "janno_final.RData" ] -->
  --  map dataParamExpVariogram [
  --    "all_distances.RData"
  --  , "binned_distances.RData"
  --  ]

  --code02Variogram "03b_nugget_estimation.R" %$
  --  [ dataPoseidonData "janno_final.RData"
  --  , dataParamExpVariogram "all_distances.RData"
  --  ] -->
  --  map dataParamExpVariogram [
  --    "lower_left_variogram.RData"
  --  , "estimated_nuggets.RData"
  --  ]

  --code02 "04a_anisotropic_mle.Rq" %$
  --  [ dataGeno "janno_final.RData" ] -->
  --  [ dataParamExp "mle_ani.RData" ]

  --code02 "04b_isotropic_mle.Rq" %$
  --  [ dataPoseidonData "janno_final.RData"
  --  , dataParamExpVariogram "estimated_nuggets.RData"
  --  ] -->
  --  [ dataParamExp "mle_iso.RData" ]
