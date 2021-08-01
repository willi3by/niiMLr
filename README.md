
<!-- README.md is generated from README.Rmd. Please edit that file -->

# niiMLr

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

The goal of niiMLr is to wrap functions from other neuroimaging and deep
learning packages to facilitate 3D neural network modeling. Many
preprocessing functions depend on an AFNI installation. For Windows
users, this will also require installation of the Windows Subsystem for
Linux (WSL). AFNI installation instructions can be found at:
<https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/background_install/install_instructs/index.html>.

## Installation

niiMLr is not yet published to CRAN, but you can install the development
version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("willi3by/niiMLr")
```

### Python

niiMLr requires Python in order to run TensorFlow. Before using niiMLr,
you may need to [tell reticulate which version of Python to
use](https://rstudio.github.io/reticulate/articles/versions.html).

<details>
<summary>
Installation on M1 Macs (click to view)
</summary>

Getting TensorFlow to work on an M1 Mac currently requires some extra
work. (The default Ananconda installation, for example, use the wrong
installation of TensorFlow.) Follow the instructions at
<https://developer.apple.com/metal/tensorflow-plugin/> to install the
correct one using miniforge, a community-driven distribution that
supports ARM.
  
Alternatively, copy and paste the following into the terminal:
``` python
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/apple/tensorflow_macos/master/scripts/download_and_install.sh)"
```
Then, use this code to tell reticulate which Python installation to use:

``` r
Sys.setenv(RETICULATE_PYTHON = paste0(Sys.getenv("HOME"), "/miniforge3/bin/python"))
```
Or:

``` r
Sys.setenv(RETICULATE_PYTHON = paste0(Sys.getenv("HOME"), "/tensorflow_macos_venv/bin/python"))
```
</details>
