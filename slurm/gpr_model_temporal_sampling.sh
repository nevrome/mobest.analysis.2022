#!/bin/bash

sbatch -p short -c 32 --mem=100G -J "gpr" -o "/projects1/coest_mobility/log/%j.gpr.out" -e "/projects1/coest_mobility/log/%j.gpr.err" --wrap="date && Rscript /projects1/coest_mobility/coest.interpol.2020/R/gpr_interpolation/02_gpr_model_temporal_sampling.R && date"
