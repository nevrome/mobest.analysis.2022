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
      dataParamExpTargeted "all_distances.RData"
    , dataParamExpTargeted "binned_distances.RData"
    , dataParamExpTargeted "lower_left_variogram.RData"
    , dataParamExpTargeted "estimated_nuggets.RData"
    , dataParamExpTargeted "mle_ani.RData"
    , dataParamExpTargeted "mle_iso.RData"
    ]

  code02 "03a_variogram_calculation.R" %$
    [ dataGeno "janno_final.RData" ] -->
    map dataParamExpTargeted [
      "all_distances.RData"
    , "binned_distances.RData"
    ]

  code02 "03b_nugget_estimation.R" %$
    [ dataGeno "janno_final.RData"
    , dataParamExpTargeted "all_distances.RData"
    ] -->
    map dataParamExpTargeted [
      "lower_left_variogram.RData"
    , "estimated_nuggets.RData"
    ]

  code02 "04a_anisotropic_mle.RC16M32" %$
    [ dataGeno "janno_final.RData" ] -->
    [ dataParamExpTargeted "mle_ani.RData" ]

  code02 "04b_isotropic_mle.RC16M32" %$
    [ dataGeno "janno_final.RData"
    , dataParamExpTargeted "estimated_nuggets.RData"
    ] -->
    [ dataParamExpTargeted "mle_iso.RData" ]
