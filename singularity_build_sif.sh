#!/bin/bash

sudo SINGULARITY_TMPDIR=$HOME/agora/mobest.analysis.2020/stempdir singularity build singularity_mobest.sif singularity_mobest.def
