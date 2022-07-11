#!/usr/bin/env stack
{- stack script
 --resolver lts-18.28
 --package shake
 -}

import ShakeUtils
import Development.Shake

main :: IO ()
main = shakeArgs myShakeOpts $ do

  want $ [
    -- plots "figure_sup_27_simulation_real_world_examples.pdf"
    plots "figure_sup_28_simulation_setup.pdf"
  , plots "figure_sup_29_simulation_scenarios.pdf"
  , plots "figure_sup_30_simulation_locate_examples.pdf"
  , plots "figure_sup_31_simulation_accuracy_permutations.pdf"
  ]

  code07 "02_define_scenarios.R" %$
    [] -> [ dataSimulation "scenarios.RData" ]

  code07 "03_create_mock_data.R" %$
    [ dataSimulation "scenarios.RData" ] -->
    [ dataSimulation "mock_data.RData" ]

  code07 "04_run_example.R" %$
    [ dataSimulation "scenarios.RData"
    , dataSimulation "mock_data.RData"
    ] -->
    [ dataSimulation "example_run.RData" ]

  code07 "05_run_permutations" %$
    [ dataSimulation "scenarios.RData"
    , dataSimulation "mock_data.RData"
    ] -->
    [ dataSimulation "permutations_accuracy_summary.RData" ]

  -- plots

  code04Paper "figure_sup_28_simulation_setup.R" %$
    [ dataSimulation "mock_data.RData" ] -->
    [ plots "figure_sup_28_simulation_setup.pdf" ]

  code04Paper "figure_sup_29_simulation_scenarios.R" %$
    [ dataSimulation "scenarios.RData"
    , dataSimulation "mock_data.RData"
    , dataSimulation "example_run.RData"
    ] -->
    [ plots "figure_sup_29_simulation_scenarios.pdf" ]

  code04Paper "figure_sup_30_simulation_locate_examples.R" %$
    [ dataSimulation "example_run.RData" ] -->
    [ plots "figure_sup_30_simulation_locate_examples.pdf" ]

  code04Paper "figure_sup_31_simulation_accuracy_permutations.R" %$
    [ dataSimulation "permutations_accuracy_summary.RData" ] -->
    [ plots "figure_sup_31_simulation_accuracy_permutations.pdf" ]

--  code07 "01_real_world_group_development.R" %$
--    [ dataGeno "janno_final.RData"
--    , dataSpatial "epsg3035.RData"
--    , ...
--    ] -->
--    [ dataSimulation "real_world_group_development.RData" ]

--  code04Paper "figure_sup_27_simulation_real_world_examples.R"