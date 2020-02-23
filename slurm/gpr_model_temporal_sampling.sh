#!/bin/bash

sbatch -p medium -c 32 --mem=200G -J "gpr" -o "/projects1/coest_mobility/log/%j.gpr.out" -e "/projects1/coest_mobility/log/%j.gpr.err" --wrap="date && Rscript /projects1/coest_mobility/coest.interpol.2020/R/gpr_interpolation/02b_run_kriging_on_cluster.R && date"
