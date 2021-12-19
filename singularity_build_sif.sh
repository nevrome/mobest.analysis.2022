#!/bin/bash

sudo SINGULARITY_TMPDIR=$HOME/agora/mobest.analysis.2022/stempdir singularity build singularity_mobest.sif singularity_mobest.def
