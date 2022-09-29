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
      dataOriginSearch "default_kernset.RData"
    , dataOriginSearch "retrospection_distances.RData"
    , dataOriginSearch "interpol_grid_selected_timeslices.RData"
    , dataOriginSearch "interpol_grid_specific_places.RData"
    , dataOriginSearch "janno_search_selected_individuals.RData"
    , dataOriginSearch "search_result_selected_individuals.RData"
    ]

  code03 "00_interpolation_and_origin_search_settings.R" %$
    [ dataParamExp "crossvalidation_best_kernels.RData" 
    ] -->
    [ dataOriginSearch "default_kernset.RData"
    , dataOriginSearch "retrospection_distances.RData"
    ]

  code03 "01_interpolation_for_selected_timeslices.R" %$
    [ dataGeno "janno_final.RData"
    , dataSpatial "extended_area.RData"
    , dataOriginSearch "default_kernset.RData"
    ] -->
    [ dataOriginSearch "interpol_grid_selected_timeslices.RData"
    ]

  code03 "02_interpolation_at_specific_places.R" %$
    [ dataGeno "janno_final.RData"
    , dataSpatial "epsg3035.RData"
    , dataOriginSearch "default_kernset.RData"
    ] -->
    [ dataOriginSearch "interpol_grid_specific_places.RData"
    ]

  code03 "03a_origin_search_for_selected_individuals.R" %$
    [ dataGeno "janno_final.RData"
    , dataSpatial "extended_area.RData"
    , dataSpatial "epsg3035.RData"
    , dataOriginSearch "default_kernset.RData"
    ] -->
    [ dataOriginSearch "janno_search_selected_individuals.RData"
    , dataOriginSearch "search_result_selected_individuals.RData"
    ]

  code03 "03b_diachronic_origin_search_for_selected_individuals.R" %$
    [ dataGeno "janno_final.RData"
    , dataSpatial "extended_area.RData"
    , dataSpatial "epsg3035.RData"
    , dataOriginSearch "default_kernset.RData"
    ] -->
    [ dataOriginSearch "diachronic_janno_search_selected_individuals.RData"
    , dataOriginSearch "diachronic_search_result_selected_individuals.RData"
    ]
