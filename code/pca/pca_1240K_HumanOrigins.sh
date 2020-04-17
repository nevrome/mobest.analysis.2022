#!/bin/bash

sbatch -p medium -c 32 --mem=100G -J "smartpca" -o "/projects1/coest_mobility/log/%j.gdm.out" -e "/projects1/coest_mobility/log/%j.gdm.err" --wrap="date && Rscript /projects1/coest_mobility/coest.interpol.2020/R/data_preparation/pca_1240K_HumanOrigins.R && date"
