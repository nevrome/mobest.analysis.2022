#!/usr/bin/env stack
{- stack script
 --resolver lts-18.28
 --package shake,filepath
 -}

import ShakeUtils
import Development.Shake

main :: IO ()
main = shakeArgs myShakeOpts $ do

  --want $ [dataGenoSnpSubUnfilteredModern "unfiltered_snp_selection_with_modern_reference_pops.bed"]

  -- 05

  code0105 "01_mds_plink_unfiltered.sh" %$
    [ dataGenoSnpSubUnfiltered "unfiltered_snp_selection.bed" ] -->
    [ dataGenoMultivarMDSUnfiltered "mds.mds" ]

  code0105 "02_mds_plink_filtered.sh" %$
    [ dataGenoSnpSubFiltered "filtered_snp_selection.bed" ] -->
    [ dataGenoMultivarMDSFiltered "mds.mds" ]

  code0105 "03_pca_smartsnp_unfiltered.Rsmartsnp" %$
    [ dataGenoSnpSubFiltered "filtered_snp_selection.bed" ] -->
    [ dataGenoMultivarPCAFiltered "pca_out.RData" ]

  code0105 "04_pca_smartsnp_filtered.Rsmartsnp" %$
    [ dataGenoSnpSubFiltered "filtered_snp_selection.bed" ] -->
    [ dataGenoMultivarPCAFiltered "pca_out.RData" ]

  code0105 "05_emu_unfiltered.shemu" %$
    [ dataGenoSnpSubFiltered "filtered_snp_selection.bed" ] -->
    [ dataGenoMultivarEMUFiltered "emu_out.txt.eigenvecs" ]

  code0105 "06_emu_filtered.shemu" %$
    [ dataGenoSnpSubFiltered "filtered_snp_selection.bed" ] -->
    [ dataGenoMultivarEMUFiltered "emu_out.txt.eigenvecs" ]

  code0105 "07_pca_smartsnp_projected_unfiltered.Rsmartsnp" %$
    [ dataGenoSnpSubFiltered "filtered_snp_selection.bed" ] -->
    [ dataGenoMultivarPCAProjFiltered "pca_out.RData" ]

  code0105 "08_pca_smartsnp_projected_filtered.Rsmartsnp" %$
    [ dataGenoSnpSubFiltered "filtered_snp_selection.bed" ] -->
    [ dataGenoMultivarPCAProjFiltered "pca_out.RData" ]
