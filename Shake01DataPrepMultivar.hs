#!/usr/bin/env stack
{- stack script
 --resolver lts-18.28
 --package shake,filepath
 -}

import ShakeUtils
import Development.Shake

main :: IO ()
main = shakeArgs myShakeOpts $ do

  want $ [dataGeno "janno_final.RData"]

  -- 05

  code0105 "01_mds_plink_unfiltered.shplink" %$
    [ dataGenoSnpSubUnfiltered "unfiltered_snp_selection.bed" ] -->
    [ dataGenoMultivarMDSUnfiltered "mds.mds" ]

  code0105 "02_mds_plink_filtered.shplink" %$
    [ dataGenoSnpSubFiltered "filtered_snp_selection.bed" ] -->
    [ dataGenoMultivarMDSFiltered "mds.mds" ]

  code0105 "03_pca_smartsnp_unfiltered.Rsmartsnp" %$
    [ dataGenoSnpSubFiltered "filtered_snp_selection.bed" ] -->
    [ dataGenoMultivarPCAUnfiltered "pca_out.RData" ]

  code0105 "04_pca_smartsnp_filtered.Rsmartsnp" %$
    [ dataGenoSnpSubFiltered "filtered_snp_selection.bed" ] -->
    [ dataGenoMultivarPCAFiltered "pca_out.RData" ]

  code0105 "05_emu_unfiltered.shemu" %$
    [ dataGenoSnpSubFiltered "filtered_snp_selection.bed" ] -->
    [ dataGenoMultivarEMUUnfiltered "emu_out.txt.eigenvecs" ]

  code0105 "06_emu_filtered.shemu" %$
    [ dataGenoSnpSubFiltered "filtered_snp_selection.bed" ] -->
    [ dataGenoMultivarEMUFiltered "emu_out.txt.eigenvecs" ]

  code0105 "07_pca_smartsnp_projected_unfiltered.Rsmartsnp" %$
    [ dataGenoSnpSubFiltered "filtered_snp_selection.bed" ] -->
    [ dataGenoMultivarPCAProjUnfiltered "pca_out.RData" ]

  code0105 "08_pca_smartsnp_projected_filtered.Rsmartsnp" %$
    [ dataGenoSnpSubFiltered "filtered_snp_selection.bed" ] -->
    [ dataGenoMultivarPCAProjFiltered "pca_out.RData" ]

  code01 "06_prepare_final_dataset.R" %$
    [ dataGenoSnpSubFiltered "filtered_snp_selection.janno"
    , dataGeno "janno_without_identicals.RData"
    , dataSpatial "epsg3035.RData"
    , dataSpatial "mobility_regions.RData"
    , dataGenoMultivarMDSUnfiltered "mds.mds"
    , dataGenoMultivarMDSFiltered "mds.mds"
    , dataGenoMultivarPCAUnfiltered "pca_out.RData"
    , dataGenoMultivarPCAFiltered "pca_out.RData"
    , dataGenoMultivarEMUUnfiltered "emu_out.txt.eigenvecs"
    , dataGenoMultivarEMUFiltered "emu_out.txt.eigenvecs"
    , dataGenoMultivarPCAProjUnfiltered "pca_out.RData"
    , dataGenoMultivarPCAProjFiltered "pca_out.RData"
    ] -->
    [ dataGeno "janno_final.RData"
    , dataGeno "multivar_perm_obs_bundles.RData"
    ]
