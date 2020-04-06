#!/bin/bash

sbatch -p short -c 1 --mem=5G -J "singularity" -o "/projects1/coest_mobility/log/%j.out" -e "/projects1/coest_mobility/log/%j.err" --wrap="date && singularity exec --bind=/projects1 /projects1/coest_mobility/coest.interpol.2020/singularity/coest.interpol.environment.sif Rscript /projects1/coest_mobility/coest.interpol.2020/R/test.R && date"
