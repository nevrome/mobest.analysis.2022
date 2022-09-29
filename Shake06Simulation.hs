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
      dataSimulation "scenarios.RData"
    , dataSimulation "mock_data.RData"
    , dataSimulation "example_run.RData"
    , dataSimulation "permutations_accuracy_summary.RData"
    ]

  code06 "01_real_world_group_development.R" %$
    [ dataGeno "janno_final.RData"
    , dataSpatial "epsg3035.RData"
    , dataOriginSearch "default_kernset.RData"
    ] -->
    [ dataSimulation "real_world_group_development.RData" ]

  code06 "02_define_scenarios.R" %$
    [] -->
    [ dataSimulation "scenarios.RData" ]

  code06 "03_create_mock_data.R" %$
    [ dataSimulation "scenarios.RData" ] -->
    [ dataSimulation "mock_data.RData" ]

  code06 "04_run_example.R" %$
    [ dataSimulation "scenarios.RData"
    , dataSimulation "mock_data.RData"
    ] -->
    [ dataSimulation "example_run.RData" ]

  code06 "05_run_permutations.RC48M50" %$
    [ dataSimulation "scenarios.RData"
    , dataSimulation "mock_data.RData"
    ] -->
    [ dataSimulation "permutations_accuracy_summary.RData" ]
