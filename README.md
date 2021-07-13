
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simulariatools

<!-- badges: start -->
[![R-CMD-check](https://github.com/Simularia/simulariatools/workflows/R-CMD-check/badge.svg)](https://github.com/Simularia/simulariatools/actions)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.165117.svg)](https://doi.org/10.5281/zenodo.596741)
<!-- badges: end -->

`simulariatools` is an open source collection of functions and tools
useful to pre and post process data for air quality modelling and
assessment. The package is developed and maintained by the people at
[Simularia](https://www.simularia.it) and it is extensively used therein
for daily jobs.

If you use this package in your work, please consider citing it. Refer
to its [Zenodo DOI](https://doi.org/10.5281/zenodo.596741) to cite it.

## Installation

Installation of `simulariatools` from GitHub can be easily done using
the `devtools` package:

``` r
    library("devtools")
    install_github("Simularia/simulariatools")
```

The same commands can also be used to upgrade the package.

We also try to keep up to date versions of the package
[here](https://www.dropbox.com/sh/71252ydg6h3xvgc/AADiWKkh_tJHK97e5-ILAQVWa?dl=0)
if you can’t build the package yourself.

Note: in order to use `importADSOBIN()` to import *ADSO/BIN* data files,
a working installation of *Python 3* is required. For more information
about *R* and *Python* interoperability, please refer to
[reticulate](https://rstudio.github.io/reticulate/) documentation.

## Brief examples

### Contour plot

Firt, import air quality data from *NetCDF* or *ADSO/BIN* files with the
appropriate convenience function:

``` r
library(simulariatools)
mydata <- importRaster(file = "./test/ave_anno_nox_all.nc",
                       k = 1000,
                       destaggering = TRUE,
                       variable = "NOX",
                       verbose = TRUE)
#> Loading required namespace: ncdf4
#> 
#> Raster statistics -----------------------------------------------
#>        X (min, max, dx)  :  278000.000   303200.000      200.000
#>        Y (min, max, dy)  : 5027000.000  5052200.000      200.000
#>      NOX (min, max, mean):    0.00e+00     6.41e+01     7.78e-01
#> -----------------------------------------------------------------
```

A quick contour plot with default customiztion can be easily obtained by
running *contourPlot2()* without any argument:

``` r
contourPlot2(mydata)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" /> The
plot is customizable by using *contourPlot2()* arguments and by piping
*ggplot2* instructions:

``` r
library(ggplot2)
contourPlot2(mydata, 
             domain = c(280000, 303000, 5028000, 5052000, 5, 5),
             levels = c(-Inf, 10, 15, 20, 30, 40),
             legend = "NOx [ug/m3]") + 
  labs(x = NULL, y = NULL) +
  theme_minimal()
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

Use `ggsave()` to save the last plot to file:

``` r
ggsave(filename = "~/path/to/myplot.png", width = 7, height = 6, dpi = 300)
```

## List of functions

Available functions are listed below:

-   `contpourPlot2()`
-   `downloadBasemap()`
-   `importRaster()`.
-   `importADSOBIN()`.
-   `importSurferGrd()`.
-   `plotAvgRad()`.
-   `plotAvgTemp()`.
-   `plotStabilityClass()`.
-   `removeOutliers()`.
-   `rollingMax()`.
-   `stabilityClass()`.
-   `vectorField()`.
-   `contourPlot()` obsolete.
-   `createBaseMap()` obsolete.

## Contact

Contact person:

    Giuseppe Carlino

    Simularia s.r.l.

    g.carlino@simularia.it

## Contributors

Matteo Paolo Costa

## License

`simulariatools` is distributed under the GPL-2 license.
