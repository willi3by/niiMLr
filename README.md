
<!-- README.md is generated from README.Rmd. Please edit that file -->

# niiMLr

<!-- badges: start -->

<!-- badges: end -->

The goal of niiMLr is to wrap functions from other neuroimaging and deep
learning packages to facilitate 3D neural network modeling. Many
preprocessing functions depend on an AFNI installation. For Windows
users, this will also require installation of the Windows Subsystem for
Linux (WSL). AFNI installation instructions can be found at:
<https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/background_install/install_instructs/index.html>.

## Installation

You can install the released version of niiMLr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("niiMLr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("willi3by/niiMLr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(niiMLr)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
