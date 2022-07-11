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
      dataParamExpMultivar "multivar_comparison_summary.RData"
    ]

  code0201 "01b_distance_correlation.RC8M50" %$
    [ dataGeno "janno_final.RData"
    , dataGeno "multivar_perm_obs_bundles.RData"
    , code0201 "01a_distance_helper_functions.R"
    ] -->
    [ dataParamExpMultivar "distance_products.RData" ]

  code0201 "02b_sge_multivar_analysis_crossval.shq" %$
    [ code0201 "02a_multivar_analysis_crossval.R"
    , dataGeno "janno_final.RData"
    , dataGeno "multivar_perm_obs_bundles.RData"
    , dataParamExpMultivar "distance_products.RData"
    ] -->
    [ dataParamExpMultivarCrossval "multivar_comparison_1.RData" ]

  code0201 "02c_modify_crossvalidation_results.R" %$
    [ dataParamExpMultivar "distance_products.RData"
    , dataParamExpMultivarCrossval "multivar_comparison_1.RData"
    , code0201 "01a_distance_helper_functions.R"
    ] -->
    [ dataParamExpMultivar "multivar_comparison_summary.RData" ]
