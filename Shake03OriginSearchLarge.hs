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
      dataOriginSearch "packed_origin_vectors.RData"
    , dataOriginSearch "origin_summary.RData"
    , dataOriginSearch "no_data_windows.RData"
    ]

--  code03 "04b_sge_large_origin_search.shq" %$
--    [ code03 "04a_large_origin_search.R"
--    , dataGeno "janno_final.RData"
--    , dataSpatial "extended_area.RData"
--    , dataOriginSearch "default_kernset.RData"
--    , dataOriginSearch "retrospection_distances.RData"
--    ] -->
--    [ dataOriginSearchLarge "ovs_sample_1.RData" ]

  code03 "04c_compile_large_origin_search_runs.R" %$
    [ dataOriginSearchLarge "ovs_sample_1.RData"
    , dataGeno "janno_final.RData"
    ] -->
    [ dataOriginSearch "packed_origin_vectors.RData"
    , dataOriginSearch "origin_summary.RData"
    , dataOriginSearch "no_data_windows.RData"
    ]
