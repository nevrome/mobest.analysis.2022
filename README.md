## Research compendium for ‘Estimating human mobility in Holocene Western Eurasia with bulk ancient genomic data’

### Compendium DOI:

<http://dx.doi.org/...>

The files at the URL above will generate the results as found in the publication. The files hosted at <https://github.com/nevrome/mobest.analysis.2020> are the development versions and may have changed since the paper was published.

### Authors of this repository:

- Clemens Schmid [![ORCiD](https://img.shields.io/badge/ORCiD-0000--0003--3448--5715-green.svg)](http://orcid.org/0000-0003-3448-5715)
- Stephan Schiffels [![ORCiD](https://img.shields.io/badge/ORCiD-0000--0002--1017--9150-green.svg)](http://orcid.org/0000-0002-1017-9150)

### Published in:

**(in Review)**: Schmid C., Schiffels S., Estimating human mobility in Holocene Western Eurasia with bulk ancient genomic data

### Overview of contents:

This repository contains the following top level directories:

- `code`: The R and shell scripts necessary to reproduce the analysis and create the figures. They are organised in subdirectories for different domains and roughly ordered with a leading number.
- `data`: The scaffold of a directory structure to hold the intermediate data output of the scripts. The actual data is too big to be uploaded here and therefore not part of the repository.
- `data_tracked`: Small input data files manually created for this analysis.
- `tables` & `plots`: Readily rendered versions of tables and plots for the publication. Figure and table numbers might deviate from the numbers in the paper.

The `DESCRIPTION` and the `.Rbuildignore` file are defining this repository as an R package. This mechanism is only used for R package dependency management, so that all necessary packages can be installed automatically (e.g. with `remotes::install_github("nevrome/mobest.analysis.2020")`). The `.Rproj` config file defines an RStudio project, only for convenient opening this repository in the [RStudio IDE](https://www.rstudio.com/products/rstudio/).

The other additional files are part of a mechanism to simplify running and reproducing the code. `singularity_mobest.def` defines a [singularity](https://singularity.hpcng.org/) container that includes all (!) software necessary to run the code in this repository. It can be build with a script like `singularity_build_sif.sh`, which requires an empty directory for temporary data `stempdir`. To run arbitrary scripts through singularity and the SGE scheduler of the HPC at the MPI-EVA, we used the script `singularity_qsub.sh`.

The `Shakefile.hs` [Haskell](https://www.haskell.org/) [stack script](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter) defines a build-pipeline for the complete analysis with the build tool [shake](https://shakebuild.com/). 

### How to reproduce:

This repository features a lot of the code and data necessary to reproduce the complete analysis of the paper. It also usually relies on relative paths and an architecture independent of a specific environment. There are a number of exceptions though, for which we can only provide partial solutions. So if you really want to rerun this analysis in its entirety you will have to apply some tweaks.

- **Data**: This analysis depends on one major dataset of genotype data with (archaeological) context information: [The Allen Ancient DNA Resource](https://reich.hms.harvard.edu/allen-ancient-dna-resource-aadr-downloadable-genotypes-present-day-and-ancient-dna-data). For our analysis we worked with Version 50 of the dataset, for which we have reason to assume it will be permanently hosted on the website of the Reich Lab. We therefore refrained from copying the large dataset to a data repository. All other datasets necessary to run the analysis are available in the `data_tracked` directory.
- **Software dependencies**: All necessary R packages and command line software tools are listed either in the `DESCRIPTION` or implicitly mentioned in the `singularity_mobest.def` file. The latter even manages a mechanism to download the software and create an independent and self-sufficient software environment (with `singularity build`). But as software develops rapidly, this will soon be downloading and installing software versions, which are not compatible any more with what we used here. That's why we pre-build a version of the singularity container (with singularity v3.6) [++++ CHECK ++++] and uploaded it here: ... [++++ CHECK ++++]. As long as singularity is available and sufficiently stable, this container should feature the exact software versions used to compile the paper.
- **High performance computing**: Some analysis and data transformation in this repository are computationally expensive. As of today, they can only be run in a high performance computing environment. The MPI-EVA provides such an environment for us, which we can access and manage with the scheduling software SGE v8.1.6. We therefore wrote wrapper scripts to submit our code specifically to this system and environment (see for example `singularity_qsub.sh`). If you want to run the respective scripts with whatever system/environment is available to you, then you would have to rewrite the respective wrapper scripts.
- **Pipeline**: For our own convenience we structured the analysis in a shake build-script (`Shakefile.hs`). It lists all scripts, their input files and their expected output. Shake constructs a pull-based build-order from this to run the whole pipeline, including download, transformation, analysis and finally plotting of data. Theoretically, it might be the most reliable way to reproduce the complete analysis, but as it depends on stack and [stackage](https://www.stackage.org/) for its Haskell dependencies, it may not be long-term stable. It's also hard-wired for our HPC environment, so some additional changes are necessary in this `Settings` datatype:

```haskell
mpiEVAClusterSettings = Settings {
    setType = Cluster
  , singularityContainer = "singularity_mobest.sif"
  , bindPath = "--bind=/mnt/archgen/users/schmid"
  , qsubSmallCommand =  "qsub -sync y -b y -cwd -q archgen.q -pe smp 8 -l h_vmem=16G -now n -V -j y -o ~/log -N small"
  , qsubMediumCommand = "qsub -sync y -b y -cwd -q archgen.q -pe smp 16 -l h_vmem=32G -now n -V -j y -o ~/log -N medium"
  , qsubScript = "qsub -sync y -N large "
}
```

So while me did our best to make this repository as accessible and reproducible as possible, we admit that there are some hurdles to overcome. We believe for most users interested in a specific part of the analysis, it might be more convenient to build some custom scripts around the `mobest` R package (<https://github.com/nevrome/mobest>), where we provide functions for the core tasks. The README there describes a minimal workflow, on top of which applications like the one for our paper can be erected.

### Licenses:

[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/) year: 2021, copyright holder: Clemens Schmid
