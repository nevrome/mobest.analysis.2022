## Research compendium for ‘Estimating human mobility in Holocene Western Eurasia with bulk ancient genomic data’

### Compendium DOI:

<http://dx.doi.org/...>

The files at the URL above will generate the results as found in the publication. The files hosted at <https://github.com/nevrome/mobest.analysis.2020> are the development versions and may have changed since the paper was published.

### Authors of this repository:

- Clemens Schmid [![ORCiD](https://img.shields.io/badge/ORCiD-0000--0003--3448--5715-green.svg)](http://orcid.org/0000-0003-3448-5715)
- Stephan Schiffels [![ORCiD](https://img.shields.io/badge/ORCiD-0000-0002-1017-9150-green.svg)](http://orcid.org/0000-0002-1017-9150)

### Published in:

**(in Review)**: Schmid C., Schiffels S., Estimating human mobility in Holocene Western Eurasia with bulk ancient genomic data

### Overview of contents:

This repository contains the following top level directories:

- `code`: The R and shell scripts necessary to reproduce the analysis and create the figures. They are organised in subdirectories for different domains and roughly ordered with a leading number.
- `data`: The scaffold of a directory structure to hold the intermediate data output of the scripts. The actual data is too big to be uploaded here and therefore not part of the repository.
- `data_tracked`: Small input data files manually created for this analysis.
- `tables` & `plots`: Readily rendered versions of tables and plots for the publication.

The `DESCRIPTION` and the `.Rbuildignore` file are defining this repository as an R package. This mechanism is only used for R package dependency management, so that all necessary packages can be installed automatically. The `.Rproj` config file defines an RStudio project, only for convenient opening this repository in the [RStudio IDE](https://www.rstudio.com/products/rstudio/).

The other additional files are part of a mechanism to simplify running and reproducing the code. `singularity_mobest.def` defines a [singularity](https://singularity.hpcng.org/) container that includes all (!) software necessary to run the code in this repository. It can be build with a script like `singularity_build_sif.sh`, which requires an empty directory for temporary data `stempdir`. To run arbitrary scripts through singularity and the SGE scheduler of the HPC at the MPI-EVA, I used the script `singularity_qsub.sh`.

The `Shakefile.hs` Haskell [stack script](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter) defines a build-pipeline for the complete analysis with the build tool [shake](https://shakebuild.com/). 

### How to reproduce:



### Licenses:

[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/) year: 2021, copyright holder: Clemens Schmid
