
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simulariatools <a href="https://www.simularia.it/simulariatools/"><img src="man/figures/logo.png" align="right" height="80" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/Simularia/simulariatools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Simularia/simulariatools/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/simulariatools)](https://CRAN.R-project.org/package=simulariatools)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/simulariatools?color=brightgreen)](https://CRAN.R-project.org/package=simulariatools)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.165117.svg)](https://doi.org/10.5281/zenodo.596741)
<!-- badges: end -->

`simulariatools` is a free collection collection of functions and tools
useful to pre and post process data for air quality modelling and
assessment. The package is developed and maintained by the people at
[Simularia](https://www.simularia.it) and it is extensively used for
their daily job.

If you use this package in your work, please consider citing it. Refer
to its [Zenodo DOI](https://doi.org/10.5281/zenodo.596741) to cite it.

## Table of Contents

- [Installation](#installation)
- [Brief examples](#brief_examples)
- [List of Functions](#list_of_functions)
- [Contact](#contact)
- [Contributors](#contributors)
- [License](https://github.com/Simularia/simulariatools/blob/master/LICENSE.md)
- [Changelog](https://github.com/Simularia/simulariatools/blob/master/NEWS.md)

## Installation

Install the released version of `simulariatools` from CRAN:

``` r
install.packages("simulariatools")
```

Or install the development version from GitHub with:

``` r
pak::pkg_install("Simularia/simulariatools")
```

Note: in order to use `importADSOBIN()` to import *ADSO/BIN* data files,
a working installation of *Python 3* is required. For more information
about *R* and *Python* interoperability, please refer to
[`reticulate`](https://rstudio.github.io/reticulate/) documentation.

## Brief examples

### Contour plot

First, import air quality data from *NetCDF* or *ADSO/BIN* files with
the appropriate convenience function:

``` r
library(simulariatools)
mydata <- importRaster(file = "./development/conc_avg.nc",
                       k = 1000,
                       destaggering = TRUE,
                       variable = "nox",
                       verbose = TRUE)
#> 
#> Raster statistics -----------------------------------------------
#>        X (min, max, dx)  :  496000.000   519250.000      250.000
#>        Y (min, max, dy)  : 4943000.000  4955250.000      250.000
#>      nox (min, max, mean):    0.00e+00     2.71e+00     1.52e-01
#> -----------------------------------------------------------------
```

A quick contour plot with default configuration can be easily obtained
by running *contourPlot2()* without any argument:

``` r
contourPlot2(mydata)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="80%" height="80%" />

The plot is customisable by using *contourPlot2()* arguments and by
piping *ggplot2* instructions:

``` r
library(ggplot2)
contourPlot2(mydata,
             domain = c(502000, 519000, 4943125, 4955125, 5, 5),
             levels = c(-Inf, 0.5, 1, 1.5, 2, Inf),
             legend = "NOx [ug/m3]") +
    labs(x = NULL, y = NULL) +
    theme_minimal()
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="80%" height="80%" />

Use `ggsave()` to save the last plot to file:

``` r
ggsave(filename = "~/path/to/myplot.png", width = 7, height = 6, dpi = 300)
```

Use `tile` optional argument to produce a plot without interpolation:

``` r
library(ggplot2)
contourPlot2(mydata,
             tile = TRUE,
             legend = "NOx [ug/m3]") +
    labs(x = NULL, y = NULL) +
    theme_minimal()
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="80%" height="80%" />

## List of functions

Available functions are listed below:

- `contpourPlot2()`
- `downloadBasemap()`
- `importRaster()`
- `importADSOBIN()`
- `importSurferGrd()`
- `plotAvgRad()`
- `plotAvgTemp()`
- `plotStabilityClass()`
- `removeOutliers()`
- `rollingMax()`
- `stabilityClass()`
- `vectorField()`
- `contourPlot()` (deprecated)
- `createBaseMap()` (deprecated)

Deprecated functions will be removed in the near future.

## Contact

Contact person:

    Giuseppe Carlino
    Simularia s.r.l.
    g.carlino@simularia.it

## Contributors

Matteo Paolo Costa
